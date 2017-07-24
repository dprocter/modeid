#' @export
#' @title Model cross-validation
#' @description Returns a four column matrix, with as many rows as there are
#' crossvalidation subsets.
#' Column 1 has a fitted randomForest for each cross-valitation subset.
#' Column 2 has the test data.frame for each cross-validation subset.
#' Column 3 has the \code{\link{confusion.matrix}} for each cross-validation subset
#' Column 4 has the \code{\link{model.acc}} for each cross-validation subset
#'
#' @param training.dataset
#' The data.frame of training data you want to cross-validate
#' @param formula
#' The formula of the model you want to use to fit the randomForest,
#' e.g. \code{true.mode~ax1.mean+spd.mean} would have the true mode being
#' classified by the mean of accelerometer axis 1 and mean speed
#' @param cv.marker
#' A numeric marker for cross-validation subsets
#' @return
#' A four column matrix (I realise this is not terribly elegant, it will probably be changed).
#' The rows contain different cross-validation subsets.
#'
#' The columns are as follows:
#'
#' 1. Fitted randomForest models for each cross-validation subset
#'
#' 2. Test data.frame for each cross-validation subset
#'
#' 3. The \code{\link{confusion.matrix}} for each cross-validation subset
#'
#' 4. The \code{\link{model.acc}} for each cross-validation subset
#'

##########################
cross.validator<-function(training.dataset,formula,cv.marker){
  total.data<-training.dataset
  total.data$cv.marker<-cv.marker

  fit<-as.list(numeric(max(cv.marker)))
  pred<-as.list(numeric(max(cv.marker)))
  datasets<-as.list(numeric(max(cv.marker)))
  conf.mats<-as.list(numeric(max(cv.marker)))
  accs<-as.list(numeric(max(cv.marker)))


  for (i in 1:max(total.data$cv.marker)){
    reduced.train<-subset(total.data,cv.marker!=i)
    test<-subset(total.data,cv.marker==i)

    fit[[i]]<-randomForest::randomForest(formula,
                                         data=reduced.train,importance=TRUE
    )
    pred[[i]]<-predict(fit[[i]],test)
    test$pred.mode<-predict(fit[[i]],test)
    datasets[[i]]<-test
    conf.mats[[i]]<-confusion.matrix(datasets[[i]]$pred.mode,datasets[[i]]$true.mode)
  }
  return(cbind(fit,datasets,conf.mats,accs))
}
