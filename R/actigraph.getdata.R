actigraph.getdata<-function(accfile, epoch.length){
  metadata<-read.csv(accfile,nrows=8)
  start.date<-strsplit(as.character(metadata[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(metadata[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")

  acc.data<-read.csv(accfile,skip=10)
  if (length(acc.data)==3){
    names(acc.data)[1:3]<-c("Axis1","Axis2","Axis3")
  } else{
    if (length(acc.data)==2){
      names(acc.data)[1:2]<-c("Axis1","Axis2")
    } else{
      if (length(acc.data)==1){
        names(acc.data)[1]<-"Axis1"
      }
    }
  }

  acc.data$date.time<-seq(start.datetime, start.datetime+(length(acc.data$Axis1)-1)*epoch.length, epoch.length)

  return(acc.data)
}
