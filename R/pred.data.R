#' @title selection of veriables neccessary for prediction
#' @description Takes merged gps/acc data, with rolling averages calculated, subsets to only 
#' those variables that are needed to determine travel mode
#' @param merged.data a dataset of merged data with rolling averages calculated
#' @details Here we assume you have processed acceleormeter data using \code{\link{process.acc}},
#'  \code{\link{gps.acc.merge}} and \code{\link{rollav.calc}}.
#'  
#'  This funciton then selects only the necessary variables for prediction, and outputs them as a matrix, 
#'  ready to be fed to a fitted xgboost model
#'  
#'  @export

pred.data<-function(merged.data){
  prediction.vars<-subset(merged.data,select=c(
                          ax1.mad.4min, ax1.c10.4min, ax1.c90.4min, ax1.skew.4min, ax1.kurt.4min, ax1.fft.4min
                          ,ax2.mad.4min, ax2.c10.4min, ax2.c90.4min, ax2.skew.4min, ax2.kurt.4min, ax2.fft.4min
                          ,ax3.mad.4min, ax3.c10.4min, ax3.c90.4min, ax3.skew.4min, ax3.kurt.4min, ax3.fft.4min
                          ,spd.mean.4min, spd.sd.4min, spd.c10.4min, spd.c90.4min
                          ,sumsnr.4min, near.train.4min, dist.next.4min, dist.last.4min
                          ,abs.acc.mean.4min, acc.sd.4min, lowsp.prop.4min))
  prediction.vars<-as.matrix(prediction.vars)
  return(prediction.vars)
}