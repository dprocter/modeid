# TODO

#' @export
#' @title Model cross-validation
#' @description Returns three column matrix, with as many rows as there are
#' crossvalidation subsets.
#' 
#' Column 1 has the fitted models
#'
#' Column 2 has the \code{\link{confusion.matrix}} for each cross-validation subset
#'
#' Column 3 has the \code{\link{model.acc}} for each cross-validation subset
#'
#' @param training.data
#' The data.frame of training data you want to cross-validate
#' @param label
#' A variable of the same length as the rows of \code{training.data}, with the true mode of travel
#' @param cv.marker
#' A numeric marker for cross-validation subsets
#' @param method
#' The method to use, currently either "randomForest" or "xgboost"
#' @param threads
#' number of threads to pass to xgboost to allow parralel computation, default is 2
#' @param nrounds
#' The number of iterations for xgboost to perform, default is 10
#' @param eta
#' The eta value to supply to xgboost, between 0 and 1, lowe values reduce overfitting, default 0.1
#' @param subsample
#' The proportion of data for xgboost toapply to each tree, smaller values reduce overfitting, default 0.2
#' @param max.depth
#' The maximum tree depth for xgboost, default is 10
#' @param verbose
#' What level of printed output you want. See ?xgboost for details, 1 is default, 0 is silent
#' @return
#' A four column matrix (I realise this is not terribly elegant, it will probably be changed).
#' The rows contain different cross-validation subsets.
#'
#' The columns are as follows:
#'
#' 1. Fitted models for each cross-validation subset
#'
#' 2. The \code{\link{confusion.matrix}} for each cross-validation subset
#'
#' 3. The \code{\link{model.acc}} for each cross-validation subset
#'

##########################
cross.validator<-function(training.data, label ,cv.marker ,method ,threads=2, nrounds=10, eta=0.1
                          , subsample=1, max.depth=10, eval_metric="merror", early_stopping_rounds = 50
                          , colsample_bytree = 1
                          ,min_child_weight=1, gamma=1, seed=NULL, verbose=1){
  
  
  total.data<-training.data
  total.data$cv.marker<-cv.marker
  total.data$true.mode<-label
  total.data<-na.omit(total.data)
  nclass<-length(levels(factor(label)))
  
  fit<-as.list(numeric(max(cv.marker)))
  conf.mats<-as.list(numeric(max(cv.marker)))
  accs<-as.list(numeric(max(cv.marker)))
  
  
  for (i in 1:max(total.data$cv.marker)){
    if (!is.null(seed)){
      set.seed(seed)
    }
    reduced.train<-subset(total.data,cv.marker!=i)
    test<-subset(total.data,cv.marker==i)
    
    for.fitting<-subset(reduced.train,select=-c(cv.marker,true.mode))
    for.pred<-subset(test,select=-c(cv.marker,true.mode))
    
    if (method=="randomForest"){
      fit[[i]]<-randomForest::randomForest(x=for.fitting,
                                           y=reduced.train$true.mode,importance=TRUE
      )
      pred<-predict(fit[[i]],test)
    }
    
    if (method=="xgboost"){
      fit[[i]]<-xgboost::xgboost(
        data=as.matrix(for.fitting)
        , label=reduced.train$true.mode
        , threads=threads
        , num.class=nclass+1
        , objective = "multi:softmax"
        , verbose=verbose
        , nrounds=nrounds
        , eta=eta
        , subsample=subsample
        , colsample_bytree = colsample_bytree
        , eval_metric = eval_metric
        , early_stopping_rounds = early_stopping_rounds
        , max.depth=max.depth
        , min_child_weight=min_child_weight
        , gamma=gamma
      )
      pred<-predict(fit[[i]],newdata=as.matrix(for.pred))
      pred<-factor(pred,labels=levels(factor(label)))
      
    }
    
    if (method=="svm"){
      fit[[i]]<-e1071::svm(x=as.matrix(for.fitting),
                   y=reduced.train$true.mode,scale=TRUE
      )
      pred<-predict(fit[[i]],as.matrix(for.pred))
    }
    
    if (method=="nbayes"){
      quantiles<-apply(for.fitting,2,FUN=function(x) quantile(na.omit(x),probs = seq(0,1,0.1)))
      quantiles[1,]<-(-1000)
      quantiles[11,]<-100000
      
      
      tr.q<-for.fitting
      te.q<-for.pred
      
      for (j in 1:length(for.fitting[1,])){
        tr.q[,j]<-cut(for.fitting[,j],breaks=quantiles[,j],labels = seq(1,10,1))
        te.q[,j]<-cut(for.pred[,j],breaks=quantiles[,j],labels = seq(1,10,1))
      }
      tr.q$true.mode<-reduced.train$true.mode
      
      net<-bnlearn::naive.bayes(tr.q, "true.mode")
      fit[[i]]<-bnlearn::bn.fit(net,data=tr.q)
      
      
      pred<-predict(fit[[i]],data=te.q)
      
    }
    
    if (method=="lda"){
      quantiles<-apply(for.fitting,2,FUN=function(x) quantile(na.omit(x),probs = seq(0,1,0.1)))
      quantiles[1,]<-(-1000)
      quantiles[11,]<-100000
      
      
      tr.q<-for.fitting
      te.q<-for.pred
      
      for (j in 1:length(for.fitting[1,])){
        tr.q[,j]<-cut(for.fitting[,j],breaks=quantiles[,j],labels = seq(1,10,1))
        te.q[,j]<-cut(for.pred[,j],breaks=quantiles[,j],labels = seq(1,10,1))
      }
      tr.q$true.mode<-reduced.train$true.mode
      
      fit[[i]]<-MASS::lda(true.mode~.,data=tr.q)
      
      lda.pred<-predict(fit[[i]],te.q)
      pred<-lda.pred$class
      
    }
    
    
    conf.mats[[i]]<-confusion.matrix(predicted=pred,observed=test$true.mode)
    accs[[i]]<-model.acc(confusion.matrix(predicted=pred,observed=test$true.mode))
  }
  return(cbind(fit,conf.mats,accs))
}
