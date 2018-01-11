#' @title Calculate rolling averages from a dataset
#' @description Takes a merged gps/acc dataset and returns the same dataset with added rolling average variables
#' @param dataset A merged gps/acc dataset
#' @param acc whether there is accelerometer data, default = TRUE
#' @param gps whether there is GPS data, default=TRUE
#' @details Here we assume you have processed accelerometer data using \code{\link{process.acc}}, 
#' merged it to GPS data using \code{\link{gps.acc.merge}}, and added variables called \emph{near.train}, 
#' \emph{dist.next.min} and \emph{dist.last.min}, possibly using \code{\link{near.train}} and 
#' \code{\link{distance.moved}}
#' 
#' This function will then take that dataset, and output the same dataset with
#'  the rolling averages neccessary to fit the model in Procter et al. 2018 added to it
#'  
#' @export

rollav.calc<-function(dataset,acc=TRUE,gps=TRUE){
  input.data<-dataset
  if (isTRUE(acc)){
    input.data$ax1.mad.4min<-zoo::rollapply(input.data$ax1.mad,fill=NA,align="center",width=25
                                              ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax1.c10.4min<-zoo::rollapply(input.data$ax1.c10,fill=NA,align="center",width=25
                                              ,partial=TRUE, FUN=function(x){quantile(x,0.1,na.rm = TRUE)})
    input.data$ax1.c90.4min<-zoo::rollapply(input.data$ax1.c90,fill=NA,align="center",width=25
                                              ,partial=TRUE, FUN=function(x){quantile(x,0.9,na.rm = TRUE)})
    input.data$ax1.fft.4min<-zoo::rollapply(input.data$ax1.fft.mean,fill=NA,align="center",width=25
                                              ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax1.skew.4min<-zoo::rollapply(input.data$ax1.skew,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax1.kurt.4min<-zoo::rollapply(input.data$ax1.kurt,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    
    input.data$ax2.mad.4min<-zoo::rollapply(input.data$ax2.mad,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax2.c10.4min<-zoo::rollapply(input.data$ax2.c10,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x){quantile(x,0.1,na.rm = TRUE)})
    input.data$ax2.c90.4min<-zoo::rollapply(input.data$ax2.c90,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x){quantile(x,0.9,na.rm = TRUE)})
    input.data$ax2.fft.4min<-zoo::rollapply(input.data$ax2.fft.mean,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax2.skew.4min<-zoo::rollapply(input.data$ax2.skew,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax2.kurt.4min<-zoo::rollapply(input.data$ax2.kurt,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    
    input.data$ax3.mad.4min<-zoo::rollapply(input.data$ax3.mad,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax3.c10.4min<-zoo::rollapply(input.data$ax3.c10,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x){quantile(x,0.1,na.rm = TRUE)})
    input.data$ax3.c90.4min<-zoo::rollapply(input.data$ax3.c90,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x){quantile(x,0.9,na.rm = TRUE)})
    input.data$ax3.fft.4min<-zoo::rollapply(input.data$ax3.fft.mean,fill=NA,align="center",width=25
                                            ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax3.skew.4min<-zoo::rollapply(input.data$ax3.skew,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$ax3.kurt.4min<-zoo::rollapply(input.data$ax3.kurt,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    
    
  }
  if (isTRUE(gps)){
    
    
    input.data$lowspeed.counter<-numeric(length(input.data[,1]))
    input.data$lowspeed.counter[input.data$speed<2 & !is.na(input.data$speed)]<-1
    input.data$lowspeed.counter[is.na(input.data$speed)]<-NA
    
    input.data$spd.mean.4min<-zoo::rollapply(input.data$speed,fill=NA,align="center",width=25
                                              ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$spd.sd.4min<-zoo::rollapply(input.data$speed,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$spd.c10.4min<-zoo::rollapply(input.data$speed,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x){quantile(x,0.1,na.rm = TRUE)})
    input.data$spd.c90.4min<-zoo::rollapply(input.data$speed,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x){quantile(x,0.9,na.rm = TRUE)})
    
    input.data$sumsnr.4min<-zoo::rollapply(input.data$sumsnr,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$near.train.4min<-zoo::rollapply(input.data$near.train,fill=NA,align="center",width=25
                                             ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    
    input.data$dist.next.min<-distance.moved(dataset = input.data,last=FALSE,time.window = 60,epoch.length = 10)
    input.data$dist.last.min<-distance.moved(dataset = input.data,last=TRUE, time.window = 60,epoch.length = 10)
    
    input.data$dist.next.4min<-zoo::rollapply(input.data$dist.next.min,fill=NA,align="center",width=25
                                           ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$dist.last.4min<-zoo::rollapply(input.data$dist.last.min,fill=NA,align="center",width=25
                                           ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    
    input.data$abs.acc.mean.4min<-zoo::rollapply(abs(input.data$acceleration),fill=NA,align="center",width=25
                                           ,partial=TRUE, FUN=function(x) mean(na.omit(x)))
    input.data$acc.sd.4min<-zoo::rollapply(input.data$acceleration,fill=NA,align="center",width=25
                                           ,partial=TRUE, FUN=function(x) sd(na.omit(x)))
    
    input.data$lowsp.prop.4min<-zoo::rollapply(input.data$lowspeed.counter,fill=NA,align="center",width=25
                                                 ,partial=TRUE, FUN=function(x){sum(na.omit(x))/length(na.omit(x))})
    
    input.data<-subset(input.data,select = -c(lowspeed.counter))
  }
  
  return(input.data)
  
}
