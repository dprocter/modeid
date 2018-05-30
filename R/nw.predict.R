#' @export
#' @title non-wear predictor
#' @description Uses a supervised machine learning model to predict non wear time for hip worn accelerometers
#' @param dataset
#' The data.frame you want nonwear predicted for
#' @return
#' Returns a variable, the same length as those in the input dataset, which is 0 if wear time
#' and 1 if nonwear 
#'
##########################

nw.predict<-function(dataset, right=TRUE){
    dataset$ax1.mad.r<-zoo::rollapply(dataset$ax1.mad, width=60, FUN=mean, partial=TRUE, align="right")
    dataset$ax2.mad.r<-zoo::rollapply(dataset$ax2.mad, width=60, FUN=mean, partial=TRUE, align="right")
    dataset$ax3.mad.r<-zoo::rollapply(dataset$ax3.mad, width=60, FUN=mean, partial=TRUE, align="right")
    dataset$ax1.c90.r<-zoo::rollapply(dataset$ax1.c90, width=60, FUN=max, partial=TRUE, align="right")
    dataset$ax2.c90.r<-zoo::rollapply(dataset$ax2.c90, width=60, FUN=max, partial=TRUE, align="right")
    dataset$ax3.c90.r<-zoo::rollapply(dataset$ax3.c90, width=60, FUN=max, partial=TRUE, align="right")
    dataset$ax1.c10.r<-zoo::rollapply(dataset$ax1.c10, width=60, FUN=min, partial=TRUE, align="right")
    dataset$ax2.c10.r<-zoo::rollapply(dataset$ax2.c10, width=60, FUN=min, partial=TRUE, align="right")
    dataset$ax3.c10.r<-zoo::rollapply(dataset$ax3.c10, width=60, FUN=min, partial=TRUE, align="right")
    dataset$ax1.g.r<-zoo::rollapply(dataset$ax1.g, width=60, FUN=sd, partial=TRUE, align="right")
    dataset$ax2.g.r<-zoo::rollapply(dataset$ax2.g, width=60, FUN=sd, partial=TRUE, align="right")
    dataset$ax3.g.r<-zoo::rollapply(dataset$ax3.g, width=60, FUN=sd, partial=TRUE, align="right")
    dataset$pred<-predict(modeid::nwmod, newdata = as.matrix(subset(dataset, select=c(ax1.mad.r, ax2.mad.r, ax3.mad.r, ax1.c90.r, ax2.c90.r, ax3.c90.r, ax1.c10.r, ax2.c10.r, ax3.c10.r
                                                                     , ax1.g.r, ax2.g.r, ax3.g.r))))
    dataset$pred.long<-dataset$pred
    for (j in 61:length(dataset[,1])){
      if (dataset$pred[j]==1){
        dataset$pred.long[(j-60):j]<-1
      }
    }

  
  return(dataset$pred.long)
}