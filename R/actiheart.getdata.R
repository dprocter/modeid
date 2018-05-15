actiheart.getdata<-function(accfile, epoch.length){
  meta<-read.csv(accfile,nrows=13)
  start.datetime<-strptime(meta[8,2],format="%d/%m/%Y %H:%M")

  acc.data<-read.csv(accfile,skip=14)
  acc.data$date.time<-seq(start.datetime, start.datetime+(length(acc.data[,1])-1)*epoch.length, epoch.length)
  acc.data$date.time.sec<-unclass(as.POSIXct(acc.data$date.time))
  names(acc.data)[2]<-"Axis1"
  acc.data<-subset(acc.data, select=-Time)
  return(acc.data)
}
