actigraph.getdata<-function(accfile, epoch.length, nonwear){

  mode.df<-data.frame(seq(0,63,1))
  names(mode.df)<-"mode"
  mode.df$Axis1<-1
  mode.df$Axis2<-rep(c(0,1),each=4,times=8)
  mode.df$Axis3<-rep(c(0,1),each=8,times=4)
  mode.df$steps<-rep(c(0,1),times=32)
  mode.df$HR<-rep(c(0,1),each=2,times=16)
  mode.df$Lux<-rep(c(0,1),each=16,times=2)
  mode.df$Incline<-rep(c(0,1),each=32)

  metadata<-read.csv(accfile,nrows=8)
  mode.reported<-as.numeric(strsplit(as.character(metadata[8,]),split=" ")[[1]][11])
  start.date<-strsplit(as.character(metadata[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(metadata[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")

  mode.df<-subset(mode.df,mode==mode.reported)

  acc.data<-read.csv(accfile,skip=10)
  names(acc.data)[1]<-"Axis1"
  col.marker<-1

  if (mode.df$Axis2[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker]<-"Axis2"
  }
  if (mode.df$Axis3[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker]<-"Axis3"
  }
  if (mode.df$steps[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker]<-"steps"
  }
  if (mode.df$HR[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker]<-"HR"
  }
  if (mode.df$Lux[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker]<-"Lux"
  }
  if (mode.df$Incline[1]==1){
    col.marker<-col.marker+1
    names(acc.data)[col.marker:(col.marker+3)]<-c("inc.off","inc.stand","inc.sit","inc.lie")
  }


  acc.data$date.time<-seq(start.datetime, start.datetime+(length(acc.data$Axis1)-1)*epoch.length, epoch.length)
  acc.data$date.time.sec<-unclass(as.POSIXct(acc.data$date.time))
  
  if(isTRUE(nonwear)){
    acc.data$zeromarker<-acc.data$Axis1+acc.data$Axis2+acc.data$Axis3
    acc.data$zeromarker[acc.data$zeromarker>0]<-(-1)
    acc.data$zeromarker<-acc.data$zeromarker+1
    acc.data$nonwear2<-zoo::rollapply(acc.data$zeromarker,FUN=sum,align="center", partial=TRUE,width=3600/epoch.length)
    acc.data$nonwear<-0
    acc.data$nonwear[acc.data$nonwear2>357]<-1
    for (i in 1:length(acc.data$nonwear)){
      j<-i-(1800/epoch.length)
      if (j<1){j<-1}
      if (acc.data$nonwear[i]==1){
        acc.data$nonwear[j:i]<-1
      }
    }
    for (i in length(acc.data$nonwear):1){
      q<-i+(1800/epoch.length)
      if (q>length(acc.data[,1])){
        q<-length(acc.data[,1])
      }
      if (acc.data$nonwear[i]==1){
        acc.data$nonwear[i:q]<-1
      }
    }
    
    acc.data<-subset(acc.data,select=-c(zeromarker, nonwear2))

  }

  return(acc.data)
}
