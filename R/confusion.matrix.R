#' @export
#' @title Confusion matrix
#' @description
#' Returns a confusion matrix, based on a predicted and observed variable
#' @param predicted
#' A factor variable of predicted mode
#' @param
#' A factor variable of observed mode
#' @return
#' A matrix of hwo predicted and observed modes correspond
#' @details
#' The output rows are the predicted modes, the output columns are the observed
#'
#' @examples
#' pred<-numeric(10)+1
#' pred[5:6]<-2
#' pred<-factor(pred,labels=c("Mode1","Mode2"))
#'
#' obs<-numeric(10)+1
#' obs[6:8]<-2
#' obs<-factor(obs,labels=c("Mode1","Mode2"))
#'
#' confusion.matrix(pred,obs)

### confusion matrix
confusion.matrix<-function(predicted,observed){
  t.modes<-levels(observed)
  conf.creator<-data.frame(predicted,observed)
  conf.mat<-matrix(nrow=length(t.modes),ncol=length(t.modes))
  rownames(conf.mat)<-t.modes
  colnames(conf.mat)<-t.modes
  for (i in 1:length(t.modes)){
    for (j in 1:length(t.modes)){
      conf.mat[i,j]<-length(conf.creator$predicted[conf.creator$predicted==t.modes[i] &
                                                     conf.creator$observed==t.modes[j]])
    }
  }
  return(conf.mat)
}

