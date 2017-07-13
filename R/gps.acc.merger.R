####TO DO
#1. Generalise to different epoch lengths
#2. Generalise to different models


#' @title Merging GPS and accelerometer files
#' @description Returns a data.frame with Accelerometer and GPS data merged by timestamp
#' @param accfile The accelerometer file to merge, a .csv
#' @param gpsfile The GPS file to merge, a .csv
#' @param participant.id
#' A unique identifier for the participant
#' @param cutoff.method
#' 1,2 or 3
#' 1=no cutoff, keep all data
#' 2=take 7 days from the first point
#' 3=if there are over 8 days, trim the first then take 7 days, if there are 8 days, trim the first and keep the rest,
#' if there are 7 or less days, keep them all
#' @param epoch.length
#' epoch.length in seconds
#'
#' @details
#' Currently only works for Actigraph accelerometer files,
#' , and Qstarz GPS device files. If you need other types contect the author
#' , they can be included
#'

#cutoff.method 1=no cut off, 2=cutoff after 7 days, 3=if there are 9 day or more, cut the first day, then keep 7
#if there are 8 days, drop the first day, if there are 7 days or less, keep all
# Merges data from an actigraph gt3x+ accelerometer and a qstarz 1000xt GPS device
#epoch length in seconds
gps.acc.merge<-function(accfile, gpsfile, participant.id,
                        cutoff.method, epoch.length){

  ###Accelerometer data
  metadata<-read.csv(accfile,nrows=8)
  start.date<-strsplit(as.character(metadata[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(metadata[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")

  acc.data<-read.csv(accfile,skip=10)
  acc.data$date.time<-seq(start.datetime, start.datetime+(length(acc.data$Axis1)-1)*epoch.length, epoch.length)
  acc.data$ID<-participant.id
  acc.data$date.time.sec<-unclass(as.POSIXct(acc.data$date.time))
  if (cutoff.method==1) {
    acc.data<-acc.data  #i.e. do nothing, just here to remond me of that
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
      acc.data<-subset(acc.data,select=-c(yearday))
      if (no.of.days>8){
        acc.data<-acc.data[1:(10080*(60/epoch.length)),] #clip to 10800(mins in 7 days)*(epochs in a minute)
      }
    }
  }




  ###GPS
  gps.data<-read.csv(gpsfile)
  gps.data$date.time<-paste(gps.data$LOCAL.DATE,gps.data$LOCAL.TIME)
  gps.data$date.time<-strptime(gps.data$date.time,format="%Y/%m/%d %H:%M:%S")
  gps.data$day<-lubridate::wday(gps.data$date.time, label = TRUE, abbr = FALSE)
  # round the gps timing to the nearest 10 seconds, so that we can match it with the accelerometry data
  lubridate::second(gps.data$date.time)<-norm.round(second(gps.data$date.time),-1)
  gps.data$date.time.sec<-unclass(as.POSIXct(gps.data$date.time))

  # a variable that will house sum of the signal to noise ratio
  gps.data$sumsnr<-NA
  # satellite info, split into strings
  sat.info<-strsplit(as.character(gps.data$SAT.INFO..SID.ELE.AZI.SNR.),split = ";",fixed=TRUE)

  # calculation of sumsnr
  for (i in 1:length(gps.data$INDEX)){
    each.snr<-numeric(length(sat.info[[i]]))
    for (j in 1:length(sat.info[[i]])){
      each.snr[j]<-as.numeric(unlist(strsplit(sat.info[[i]][j],split="-"))[4])
    }
    gps.data$sumsnr[i]<-sum(each.snr)
  }


  # conversion of the all positive lat and long to +- NS EW
  gps.data$lat<-gps.data$LATITUDE
  gps.data$lat[gps.data$N.S==" S"]<-gps.data$lat[gps.data$N.S==" S"]*-1

  gps.data$long<-gps.data$LONGITUDE
  gps.data$long[gps.data$E.W==" W"]<-gps.data$long[gps.data$E.W==" W"]*-1

  # getting rid of the units from speed
  gps.data$spd<-NA
  for (i in 1:length(gps.data$INDEX)){
    gps.data$spd[i]<-as.numeric(unlist(strsplit(as.character(gps.data$SPEED[i]),split=" "))[2])
  }

  gps.data<-subset(gps.data,select=c(INDEX,date.time,day,date.time.sec,lat,long,spd,PDOP,HDOP,VDOP,sumsnr,LOCAL.DATE,LOCAL.TIME))
  names(gps.data)<-c("index","date.time","day","date.time.sec","latitude","longitude","speed","pdop","hdop","vdop","sumsnr","date.txt","time.txt")

  #Stick the two together, kepping all accelerometer data, the missing GPS variables will be NA
  merged.data<-merge(acc.data,gps.data,by="date.time.sec",all.x = TRUE)
  merged.data<-subset(merged.data,select=-c(date.time.y))
  names(merged.data)[5]<-"date.time"
  merged.data$prev.time<-merged.data$date.time
  merged.data$prev.time[1]<-NA
  merged.data$prev.time[2:length(merged.data[,1])]<-merged.data$date.time[1:(length(merged.data[,1])-1)]
  merged.data$remove.dups<-numeric(length(merged.data[,1]))
  merged.data$remove.dups[merged.data$date.time==merged.data$prev.time]<-1
  merged.data<-subset(merged.data,remove.dups!=1)
  merged.data<-subset(merged.data,select=-c(remove.dups,prev.time))
  #merged.data<-subset(merged.data,select=-c(date.time.sec))
  #names(merged.data)[5]<-"date.time"

  return(merged.data)
}
