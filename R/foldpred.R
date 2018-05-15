#' @export
#' @title Predict modal travel mode from cross-validated model
#' @description Returns a factor of the length of the data with predicted travel mode
#' @param data
#' The data to predict to, make sure the correct variables are selected using \code{\link{pred.data}}
#' @param model
#' The cross-validated model, created with \code{\link{cross.validator}}
#' @return
#' A factor variable of the same length as variables in the data, with predicted travel mode

##########################
foldpred<-function(data, model){
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  folds<-length(model[,1])
  
  allpreds<-data.frame(numeric(length(data[,1])))
  for (i in 1:folds){
    pred<-predict(model[,1][[i]], newdata=as.matrix(modeid::pred.data(data)))
    pred<-factor(pred, labels = c("cycle","stat","train","vehicle","walk"))
    allpreds<-cbind(allpreds,pred)
  }
  
  modal.mode<-apply(allpreds[2:length(allpreds)],1,FUN=Mode)
  modal.mode<-factor(modal.mode)
  return(modal.mode)
}