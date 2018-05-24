time.since.last<-function(datetime.variable,format){
  if (format=="DT"){
    time.base<-datetime.variable
  }else{
    time.base<-strptime(datetime.variable,format=format)
  }
  
  time.base<-as.POSIXct(time.base)
  time.base<-unclass(time.base)

  last.timepoint<-time.base
  last.timepoint[1]<-NA
  last.timepoint[2:length(last.timepoint)]<-time.base[1:length(time.base)-1]

  time.since<-(time.base-last.timepoint)
  return(time.since)
}
