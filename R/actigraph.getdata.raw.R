actigraph.getdata.raw<-function(accfile, epoch.length, samples.per.second, participant.id){

  file.data<-read.csv(accfile,nrows=8)
  mode.reported<-as.numeric(strsplit(as.character(file.data[8,]),split=" ")[[1]][11])
  start.date<-strsplit(as.character(file.data[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(file.data[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")

  this.file<-read.csv(accfile,skip=10)

  this.file$second<-rep(seq(1,length(this.file[,1])/samples.per.second),each=samples.per.second)
  if(norm.round(this.file$second[length(this.file[,1])],-1) != this.file$second[length(this.file[,1])]){
    this.file<-subset(this.file,second<=norm.round(this.file$second[length(this.file[,1])],-1)-epoch.length)
  }

  epoch.width<-epoch.length*samples.per.second
  this.file$epoch<-rep(seq(0,length(this.file[,1])/samples.per.second-epoch.length,epoch.length),each=epoch.width)

  this.file$date.time<-start.datetime+this.file$epoch

  print("Raw file read in, calculating summaries per epoch")



  Axis1<-wapply(this.file[,1], epoch.width, FUN=median ,by = epoch.width)
  raw.fft<-data.frame(Axis1)
  raw.fft$ax1.mad<-wapply(this.file[,1], epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax1.c90<-wapply(this.file$Accelerometer.X, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax1.c10<-wapply(this.file$Accelerometer.X, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)

  raw.fft$Axis2<-wapply(this.file[,2], epoch.width, FUN=median ,by = epoch.width)
  raw.fft$ax2.mad<-wapply(this.file[,2], epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax2.c90<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax2.c10<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)

  raw.fft$Axis3<-wapply(this.file[,3], epoch.width, FUN=median ,by = epoch.width)
  raw.fft$ax3.mad<-wapply(this.file[,3], epoch.width, FUN=mad ,by = epoch.width)
  raw.fft$ax3.c90<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
  raw.fft$ax3.c10<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)

  raw.fft$date.time<-seq(start.datetime,(start.datetime+length(raw.fft[,1])*epoch.length)-epoch.length,epoch.length)
  raw.fft$id<-participant.id

  print("Summaries calculated, calculating FFTs")

  strength.fft<-function(x){
    cs<-fft(x)
    cs <- cs / length(cs)

    distance.center <- function(c)signif( Mod(c),4)
    return(sapply(cs, distance.center))
  }


  ax1.fft<-wapply(this.file$Accelerometer.X, epoch.width, FUN=function (x){strength.fft(x) } ,by = epoch.width)
  ax1.fft<-t(ax1.fft)
  ax1.fft<-as.data.frame(ax1.fft)
  names(ax1.fft)<-seq(1,epoch.width,1)
  raw.fft$ax1.fft.mean<-apply(ax1.fft,1,mean)
  raw.fft$ax1.fft.max.name<-apply(ax1.fft,1,FUN=function(x) colnames(ax1.fft[which.max(x)]))

  ax2.fft<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function (x){strength.fft(x) } ,by = epoch.width)
  ax2.fft<-t(ax2.fft)
  ax2.fft<-as.data.frame(ax2.fft)
  names(ax2.fft)<-seq(1,epoch.width,1)
  raw.fft$ax2.fft.mean<-apply(ax2.fft,1,mean)
  raw.fft$ax2.fft.max.name<-apply(ax2.fft,1,FUN=function(x) colnames(ax2.fft[which.max(x)]))

  ax3.fft<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function (x){strength.fft(x)} ,by = epoch.width)
  ax3.fft<-t(ax3.fft)
  ax3.fft<-as.data.frame(ax3.fft)
  names(ax2.fft)<-seq(1,epoch.width,1)
  raw.fft$ax3.fft.mean<-apply(ax3.fft,1,mean)
  raw.fft$ax3.fft.max.name<-apply(ax3.fft,1,FUN=function(x) colnames(ax3.fft[which.max(x)]))

  raw.fft$cor.xy<-wapply.xy(x=this.file$Accelerometer.X,y=this.file$Accelerometer.Y,width=epoch.width,by=epoch.width,FUN=cor)
  raw.fft$cor.xz<-wapply.xy(x=this.file$Accelerometer.X,y=this.file$Accelerometer.Z,width=epoch.width,by=epoch.width,FUN=cor)
  raw.fft$cor.yz<-wapply.xy(x=this.file$Accelerometer.Y,y=this.file$Accelerometer.Z,width=epoch.width,by=epoch.width,FUN=cor)

  return(raw.fft)
}
