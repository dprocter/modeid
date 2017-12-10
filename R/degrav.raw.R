#epoch length in seconds
#' @export
degrav.raw<-function(accfile, epoch.length, samples.per.second, participant.id){
  
  
  
  file.data<-read.csv(accfile,nrows=8)
  mode.reported<-as.numeric(strsplit(as.character(file.data[8,]),split=" ")[[1]][11])
  start.date<-strsplit(as.character(file.data[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(file.data[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")
  
  this.file<-read.csv(accfile,skip=10)
  
  # this checks we end at a whole second, and if we don't rounds to the nearest second
  if (norm.round(length(this.file[,1])/samples.per.second,0)!=length(this.file[,1])/samples.per.second){
    new.end<-(norm.round(length(this.file[,1])/samples.per.second,0)-1)*samples.per.second
    this.file<-this.file[1:new.end,]
  }
  
  this.file$second<-rep(seq(1,length(this.file[,1])/samples.per.second),each=samples.per.second)
  
  # this truncates the data to the nearest epoch
  if(norm.round(this.file$second[length(this.file[,1])],-1) != this.file$second[length(this.file[,1])]){
    this.file<-subset(this.file,second<=norm.round(this.file$second[length(this.file[,1])],-1)-epoch.length)
  }
  
  print("Raw file read in, removing gravity")
  ##############
  grav.adjuster<-function(x){
    newgrav<-0.1*x+grav*0.9
    #assign(x="grav",value=newgrav, envir = .GlobalEnv)
    grav<<-newgrav
    return(newgrav)
  }
  
  ################################
  # gravity removal
  this.file$ax1.g<-0
  this.file$ax2.g<-0
  this.file$ax3.g<-0
  
  grav<-0
  this.file$ax1.g<-sapply(this.file$Accelerometer.X,FUN=grav.adjuster)
  grav<-0
  this.file$ax2.g<-sapply(this.file$Accelerometer.Y,FUN=grav.adjuster)
  grav<-0
  this.file$ax3.g<-sapply(this.file$Accelerometer.Z,FUN=grav.adjuster)
  
  this.file$ax1.g2<-this.file$Accelerometer.X-this.file$ax1.g
  this.file$ax2.g2<-this.file$Accelerometer.Y-this.file$ax2.g
  this.file$ax3.g2<-this.file$Accelerometer.Z-this.file$ax3.g
  
  
  this.file$vmag.g2<-sqrt(this.file$ax1.g2^2+this.file$ax2.g2^2+this.file$ax3.g2^2)
  
  
  epoch.width<-epoch.length*samples.per.second
  this.file$epoch<-rep(seq(0,length(this.file[,1])/samples.per.second-epoch.length,epoch.length),each=epoch.width)
  
  this.file$date.time<-start.datetime+this.file$epoch
  
  
  print("Gravity removed, calculating summaries per epoch")
  
  Axis1.ng<-wapply(this.file$ax1.g2, epoch.width, FUN=mean ,by = epoch.width)
  raw.fft<-data.frame(Axis1.ng)
  raw.fft$ax1.ng.mad<-wapply(this.file$ax1.g2, epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax1.ng.c90<-wapply(this.file$ax1.g2, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax1.ng.c10<-wapply(this.file$ax1.g2, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
  raw.fft$abs.ax1<-wapply(abs(this.file$ax1.g2), epoch.width, FUN=mean ,by = epoch.width)
  
  raw.fft$Axis2.ng<-wapply(this.file$ax2.g2, epoch.width, FUN=mean ,by = epoch.width)
  raw.fft$ax2.ng.mad<-wapply(this.file$ax2.g2, epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax2.ng.c90<-wapply(this.file$ax2.g2, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax2.ng.c10<-wapply(this.file$ax2.g2, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
  raw.fft$abs.ax2<-wapply(abs(this.file$ax2.g2), epoch.width, FUN=mean ,by = epoch.width)
  
  raw.fft$Axis3.ng<-wapply(this.file$ax3.g2, epoch.width, FUN=mean ,by = epoch.width)
  raw.fft$ax3.ng.mad<-wapply(this.file$ax3.g2, epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax3.ng.c90<-wapply(this.file$ax3.g2, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax3.ng.c10<-wapply(this.file$ax3.g2, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
  raw.fft$abs.ax3<-wapply(abs(this.file$ax3.g2), epoch.width, FUN=mean ,by = epoch.width)
  
  
  raw.fft$vmag.mean<-wapply(this.file$vmag.g2, epoch.width, FUN=mean ,by = epoch.width)
  raw.fft$vmag.mad<-wapply(this.file$vmag.g2, epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$vmag.c90<-wapply(this.file$vmag.g2, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$vmag.c10<-wapply(this.file$vmag.g2, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
  
  raw.fft$date.time<-seq(start.datetime,(start.datetime+length(raw.fft[,1])*epoch.length)-epoch.length,epoch.length)
  raw.fft$id<-participant.id
  
  raw.fft$date.time.sec<-unclass(as.POSIXct(raw.fft$date.time))
  
  return(raw.fft)
  
}