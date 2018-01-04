####TO DO
#1. Generalise to different models
#3. Edit timetxt to work with


#' @title Merging GPS and accelerometer files
#' @description Returns a data.frame with Accelerometer and GPS data merged by timestamp
#' @param accfile The accelerometer file to merge, a .csv
#' @param gpsfile The GPS file to merge, a .csv
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
#' @param british.time
#' whether or not we the study is in Britain, so we need to check if the data was collected within BST and adjust GPS
#' UTC timings by 1hour
#'
#' 1= need to account for BST, 0= do not
#' @param acc.model
#' The accelerometer model. Currecntly "Actigraph" and "Actiheart" are valid
#' @param raw
#' Boolean, if TRUE you are using raw data, not counts, must be exported as .csv, not the .gt3x, or .agd files
#' @param samples.per.second
#' The sampling rate of raw data if used, default is 30Hz, adjust if required
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
#' @export
gps.acc.merge<-function(accfile, gpsfile, participant.id,
                             cutoff.method, epoch.length, british.time
                             , acc.model,raw=FALSE,samples.per.second=30){

  ###Accelerometer data
  if (acc.model=="Actigraph"){
    if (raw==FALSE){
      acc.data<-actigraph.getdata(accfile=accfile,epoch.length=epoch.length)
    } else{
      acc.data<-actigraph.getdata.raw(accfile=accfile,epoch.length=epoch.length,samples.per.second=samples.per.second
                                      ,participant.id = participant.id)
    }

  } else{
    if (acc.model=="Actiheart"){
      #start.date<-actiheart.getmeta(accfile)
      acc.data<-actiheart.getdata(accfile=accfile, epoch.length=epoch.length)
    } else{
      stop("Don't recognise the accelerometer model, currently only Actigraph and Actiheart are valid models")
    }
  }

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
      if (no.of.days>8){
        acc.data<-acc.data[1:(10080*(60/epoch.length)),] #clip to 10800(mins in 7 days)*(epochs in a minute)
      }
    }
  }
  if (!is.null(acc.data$yearday)){
    acc.data<-subset(acc.data,select = -c(yearday))
  }




  ###GPS
  if (!file.exists(gpsfile)){
    print(paste("no gps file for",ids[i],sep=" "))
  } else{

  gps.data<-read.csv(gpsfile)
  if (length(gps.data[,1])>0){
    gps.data<-subset(gps.data,VALID!=" NO FIX")
    if (length(gps.data[,1])>0){
  gps.data$date.time<-paste(gps.data$UTC.DATE,gps.data$UTC.TIME)
  gps.data$date.time<-strptime(gps.data$date.time,format="%Y/%m/%d %H:%M:%S")
  gps.data<-subset(gps.data,UTC.DATE!=" ")

  # checking whether the file is within BST and adjusting
  if (british.time==1){
    BSTstarts<-c("25/03/2012 00:00:00", "31/03/2013 00:00:00","30/03/2014 00:00:00"
                 , "29/03/2015 00:00:00", "27/03/2016 00:00:00", "26/03/2017 00:00:00")
    BSTends<-c("28/10/2012 00:00:00", "27/10/2013 00:00:00", "26/10/2014 00:00:00"
               , "25/10/2015 00:00:00", "30/10/2016 00:00:00", "29/10/2017 00:00:00")
    BSTstarts<-strptime(BSTstarts,format="%d/%m/%Y %H:%M:%S")
    BSTends<-strptime(BSTends,format="%d/%m/%Y %H:%M:%S")
    within.st<-0

    for (z in 1:length(BSTstarts)){
      if (gps.data$date.time[1]>=BSTstarts[z] & gps.data$date.time[1]<=BSTends[z]){
        within.st<-1
      }
    }

    if (within.st==1){
      gps.data$date.time<-gps.data$date.time+3600
    }

  } else{
    gps.data$date.time<-paste(gps.data$LOCAL.DATE,gps.data$LOCAL.TIME)
    gps.data$date.time<-strptime(gps.data$date.time,format="%Y/%m/%d %H:%M:%S")
  }
  gps.data$day<-lubridate::wday(gps.data$date.time, label = TRUE, abbr = FALSE)

  if (epoch.length==5){
    lubridate::second(gps.data$date.time)<-round(lubridate::second(gps.data$date.time)/5)*5
  }
  # rounding gps data to the nearest epoch
  if (epoch.length==10){
    # round the gps timing to the nearest 10 seconds, so that we can match it with the accelerometry data
    lubridate::second(gps.data$date.time)<-norm.round(lubridate::second(gps.data$date.time),-1)
  }

  if (epoch.length==15){
    lubridate::second(gps.data$date.time)<-round(lubridate::second(gps.data$date.time)/15)*15
  }


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

  # getting rid of the units from speed and height
  gps.data$spd<-NA
  gps.data$hi<-NA
  for (i in 1:length(gps.data$INDEX)){
    gps.data$spd[i]<-as.numeric(unlist(strsplit(as.character(gps.data$SPEED[i]),split=" "))[2])
    gps.data$hi[i]<-as.numeric(unlist(strsplit(as.character(gps.data$HEIGHT[i]),split=" "))[2])
  }
  gps.data$date.time.sec<-unclass(as.POSIXct(gps.data$date.time))


  gps.data<-subset(gps.data,select=c(INDEX,date.time,date.time.sec,day,lat,long,spd,hi,PDOP,HDOP,VDOP,sumsnr))
  names(gps.data)<-c("index","date.time","date.time.sec","day","latitude","longitude","speed","height","pdop","hdop","vdop","sumsnr")

  #Stick the two together, kepping all accelerometer data, the missing GPS variables will be NA
  merged.data<-merge(acc.data,gps.data,by="date.time.sec",all.x = TRUE)
  #merged.data<-subset(merged.data,select=-c(date.time.y))
  #names(merged.data)[5]<-"date.time"
  merged.data$date.time<-merged.data$date.time.x
  merged.data<-subset(merged.data,select=-c(date.time.x,date.time.y))
  merged.data$prev.time<-merged.data$date.time
  merged.data$prev.time[1]<-NA
  merged.data$prev.time[2:length(merged.data[,1])]<-merged.data$date.time[1:(length(merged.data[,1])-1)]
  merged.data$remove.dups<-numeric(length(merged.data[,1]))
  merged.data$remove.dups[merged.data$date.time==merged.data$prev.time]<-1
  merged.data<-subset(merged.data,remove.dups!=1)
  merged.data<-subset(merged.data,select=-c(remove.dups,prev.time,date.time.sec))

  merged.data$acceleration<-NA
  for (j in 2:length(merged.data[,1])){
    if (!is.na(merged.data$speed[j]) & !is.na(merged.data$speed[j-1])){
      merged.data$acceleration[j]<-(merged.data$speed[j]-merged.data$speed[j-1])/epoch.length
    }
  }

  return(merged.data)
    }
  } else{
    print(paste(participant.id, "has no GPS data, ignoring", sep=" "))
  }
  }
}
