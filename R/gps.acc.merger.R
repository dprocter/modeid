require(lubridate)

# Merges data from an actigraph gt3x+ accelerometer and a qstarz 1000xt GPS device
gps.acc.merge<-function(accfile,gpsfile,participant.id){

  ###Accelerometer data

  metadata<-read.csv(accdata,nrows=8)
  start.date<-strsplit(as.character(metadata[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(metadata[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")


  acc.data<-read.csv(accdata,skip=10)
  acc.data$date.time<-seq(start.datetime,start.datetime+(length(input.data$Axis1)-1)*10,10)
  acc.data$pupilid<-pupilid
  acc.data$date.time.sec<-unclass(as.POSIXct(acc.data$date.time))

  ###GPS
  gps.data<-read.csv(gpsdata)
  gps.data$date.time<-paste(gps.data$LOCAL.DATE,gps.data$LOCAL.TIME)
  gps.data$date.time<-strptime(gps.data$date.time,format="%Y/%m/%d %H:%M:%S")
  # round the gps timing to the nearest 10 seconds, so that we can match it with the accelerometry data
  second(gps.data$date.time)<-round2(second(gps.data$date.time),-1)
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

  gps.data<-subset(gps.data,select=c(INDEX,date.time,date.time.sec,lat,long,spd,PDOP,HDOP,VDOP,sumsnr))
  names(gps.data)<-c("index","date.time","date.time.sec","latitude","longitude","speed","pdop","hdop","vdop","sumsnr")

  #Stick the two together, kepping all accelerometer data, the missing GPS variables will be NA
  merged.data<-merge(input.data,gps.data,by.x="date.time.sec",by.y="date.time.sec",all.x = TRUE)
  merged.data<-subset(merged.data,select=-c(date.time.y))
  names(merged.data)[5]<-"date.time"
  merged.data$ID<-participant.id

  return(merged.data)
}
