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
cross.validator<-function(training.data, label ,cv.marker ,method ,threads=2, nrounds=10, eta=0.1, subsample=0.2, max.depth=10
                   ,min_child_weight=1, gamma=1, seed=NULL){
  
  
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
                                           y=label,importance=TRUE
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
        , verbose=1
        , nrounds=nrounds
        , eta=eta
        , subsample=subsample
        , max.depth=max.depth
        , min_child_weight=min_child_weight
        , gamma=gamma
      )
      pred<-predict(fit[[i]],newdata=as.matrix(for.pred))
      pred<-factor(pred,labels=levels(factor(label)))
      
    }
    
    
    conf.mats[[i]]<-confusion.matrix(predicted=pred,observed=test$true.mode)
    accs[[i]]<-model.acc(confusion.matrix(predicted=pred,observed=test$true.mode))
  }
  return(cbind(fit,conf.mats,accs))
}
