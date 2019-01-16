
#' @title Processing of accelerometer data files
#' @description Returns a data.frame with Accelerometer data summarised to a defined epoch
#' @param accfile The accelerometer file to merge, a .csv
#' @param participant.id
#' A unique identifier for the participant
#' @param cutoff.method
#' 1,2 or 3
#'
#' 1=no cutoff, keep all data
#'
#' 2=take 7 days from the first point
#'
#' 3=if there are over 8 days, trim the first then take 7 days, if there are 8 days, trim the first and keep the rest,
#' if there are 7 or less days, keep them all
#' @param epoch.length
#' epoch.length in seconds, currently, 5, 10 or 15 seconds only tested
#'
#' 1= need to account for BST, 0= do not
#' @param acc.model
#' The accelerometer model. Currently "Actigraph" and "Actiheart" are valid
#' @param raw
#' Boolean, if TRUE you are using raw data, not counts, must be exported as .csv, not the .gt3x, or .agd files
#' @param samples.per.second
#' The sampling rate of raw data if used, default is 30Hz, adjust if required
#'
#' @details
#' Currently only works for Actigraph or Actiheart accelerometer files,
#' If you need other types contect the author
#' , they can be included
#'
#' @export
process.acc<-function(accfile
                      , participant.id
                      , cutoff.method
                      , epoch.length
                      , acc.model
                      , raw=FALSE
                      , samples.per.second=30
                      , nonwear=TRUE
                      , nonwear.method="ML"){
  
  ###Accelerometer data
  if (acc.model=="Actigraph"){
    if (raw==FALSE){
      acc.data<-actigraph.getdata(accfile=accfile,epoch.length=epoch.length, nonwear=nonwear, participant.id = participant.id)
      
      
    } else{
      print("reading in raw accelerometer data")
      acc.data<-actigraph.getdata.raw(accfile=accfile,epoch.length=epoch.length,samples.per.second=samples.per.second
                                      ,participant.id = participant.id, nonwear=nonwear,nonwear.method=nonwear.method)
    }
    
  } else{
    if (acc.model=="Actiheart"){
      #start.date<-actiheart.getmeta(accfile)
      acc.data<-actiheart.getdata(accfile=accfile, epoch.length=epoch.length, participant.id = participant.id)
    } else{
      stop("Don't recognise the accelerometer model, currently only Actigraph and Actiheart are valid models")
    }
  }
  
  #acc.data$ID<-participant.id
  #acc.data$date.time.sec<-unclass(as.POSIXct(acc.data$date.time))
  
  acc.data$day.order<-numeric(length(acc.data[,1]))+1
  for (j in 2:length(acc.data[,1])){
    if (lubridate::wday(acc.data$date.time[j])!=lubridate::wday(acc.data$date.time[j-1])){
      acc.data$day.order[j]<-acc.data$day.order[j-1]+1
    } else{
      acc.data$day.order[j]<-acc.data$day.order[j-1]
    }
  }
  
  print("cutoff time")
  if (cutoff.method==1) {
    acc.data<-acc.data  #i.e. do nothing, just here to remind me of that
  }
  
  if (cutoff.method==2){
    acc.data<-acc.data[1:(10080*(60/epoch.length)),] #clip to 10080(mins in 7 days)*(epochs in a minute)
  }
  
  #cutoff method 3
  if (cutoff.method==3){
    acc.data$yearday<-lubridate::yday(acc.data$date.time)
    no.of.days<-length(levels(factor(acc.data$yearday)))
    if (no.of.days>7){
      acc.data<-subset(acc.data,acc.data$yearday!=acc.data$yearday[1])#get rid of day 1
      if (no.of.days>8){
        acc.data<-acc.data[1:(10080*(60/epoch.length)),] #clip to 10800(mins in 7 days)*(epochs in a minute)
      }
    }
  }
  if (!is.null(acc.data$yearday)){
    acc.data<-subset(acc.data,select = -c(yearday))
  }
  print("done")
  return(acc.data)
}

