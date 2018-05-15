actigraph.getdata.raw<-function(accfile, epoch.length, samples.per.second, participant.id, nonwear, remove.gravity=TRUE){
  
  require(foreach)
  
  ggir.calib<-GGIR::g.calibrate(accfile,windowsizes = c(epoch.length,900,3600),printsummary = FALSE)
  
  epoch.width<-epoch.length*samples.per.second
  
  # extract metadata from the file
  file.data<-read.csv(accfile,nrows=8)
  mode.reported<-as.numeric(strsplit(as.character(file.data[8,]),split=" ")[[1]][11])
  start.date<-strsplit(as.character(file.data[3,]),split=" ")[[1]][3]
  start.time<-strsplit(as.character(file.data[2,]),split=" ")[[1]][3]
  start.datetime<-paste(start.date,start.time)
  start.datetime<-strptime(start.datetime,format="%d/%m/%Y %H:%M:%S")
  
  
  # the number of lines in the file
  len.file<-R.utils::countLines(accfile)[1]
  
  # check if there are some samples that don't make up an epoch, then trim them off
  unusable<-(len.file-11)%%(epoch.length*samples.per.second)
  len.file<-len.file-unusable
  
  # count the number of 1.2 million length blocks we will need
  n.blocks<-len.file%/%1.2e6
  
  # count how big the last block will need to be to finish the file
  leftovers<-len.file%%1.2e6
  
  #make a variable called blocks, which goes from 1 to the last block
  blocks<-seq(1,n.blocks+1,1)
  # make a variable, with the row we want to start each block from
  start.markers<-seq(1,n.blocks*1.2e6+1,1.2e6)
  #####
  #EDITING 15/05/18
  start.markers[2:length(start.markers)]<-start.markers[2:length(start.markers)]-1.2e5
  rows.to.read<-numeric(length(start.markers))+1.32e6
  rows.to.read[1]<-1.2e6
  
  # find out how many cores there are on the machine
  n.cores<-parallel::detectCores()
  
  # create a cluster of the number of cores on the machine, and tell the cluster where libraries are
  cl<-parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
  
  print(paste(participant.id,"Raw file read in, calculating summaries per epoch"))
  
  out<-as.list(numeric(length(blocks)))
  
  
  # a foreach loop that summarises the accelerometer data in blocks, using way less ram than doing it all at once
  # creates a list of data.frames as output
  out<-foreach::foreach (i=1:length(blocks), .packages = c("modeid", "moments")) %dopar%{

    if (i <length(blocks)){
      this.file<-read.csv(accfile, skip = 10+start.markers[i],nrows = rows.to.read[i] )
    } else{
      this.file<-read.csv(accfile, skip = 10+start.markers[i], nrows = -1 )
    }

    
    # name the variables
    names(this.file)<-c("Accelerometer.X","Accelerometer.Y","Accelerometer.Z")
    
    # calibrate the variables
    
    this.file$Accelerometer.X<-this.file$Accelerometer.X+ggir.calib$offset[1]
    this.file$Accelerometer.Y<-this.file$Accelerometer.Y+ggir.calib$offset[2]
    this.file$Accelerometer.Z<-this.file$Accelerometer.Z+ggir.calib$offset[3]
    
    this.file$Accelerometer.X<-this.file$Accelerometer.X/ggir.calib$scale[1]
    this.file$Accelerometer.Y<-this.file$Accelerometer.Y/ggir.calib$scale[2]
    this.file$Accelerometer.Z<-this.file$Accelerometer.Z/ggir.calib$scale[3]
    
    
    # calculate vector magnitude
    this.file$vec.mag<-sqrt(this.file$Accelerometer.X^2+
                              this.file$Accelerometer.Y^2+
                              this.file$Accelerometer.Z^2)
  
    
    if (isTRUE(remove.gravity)){
      ##############
      grav.adjuster<-function(x){
        newgrav<-0.05*x+grav*0.95
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
      
      this.file$Accelerometer.X<-this.file$Accelerometer.X-this.file$ax1.g
      this.file$Accelerometer.Y<-this.file$Accelerometer.Y-this.file$ax2.g
      this.file$Accelerometer.Z<-this.file$Accelerometer.Z-this.file$ax3.g
      
    } else{
    }
    
    if (i>1){
      this.file<-this.file[120001:length(this.file[,1]),]
    }
    
    # calculate euclidean norm minus 1
    ENMO<-wapply(this.file$vec.mag, epoch.width, FUN=mean ,by = epoch.width)
    ENMO<-ENMO-1
    ENMO[ENMO<0]<-0
    
    
    Axis1<-wapply(this.file$Accelerometer.X, epoch.width, FUN=mean ,by = epoch.width)
    raw.fft<-data.frame(Axis1)
    raw.fft$ax1.mad<-wapply(this.file$Accelerometer.X, epoch.width, FUN=mad ,by = epoch.width)
    raw.fft$ax1.c90<-wapply(this.file$Accelerometer.X, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax1.c10<-wapply(this.file$Accelerometer.X, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax1.kurt<-wapply(this.file$Accelerometer.X, epoch.width, FUN=moments::kurtosis ,by = epoch.width)
    raw.fft$ax1.skew<-wapply(this.file$Accelerometer.X, epoch.width, FUN=moments::skewness ,by = epoch.width)
    
    raw.fft$Axis2<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=mean ,by = epoch.width)
    raw.fft$ax2.mad<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=mad ,by = epoch.width)
    raw.fft$ax2.c90<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax2.c10<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax2.kurt<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=moments::kurtosis ,by = epoch.width)
    raw.fft$ax2.skew<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=moments::skewness ,by = epoch.width)
    
    raw.fft$Axis3<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=mean ,by = epoch.width)
    raw.fft$ax3.mad<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=mad ,by = epoch.width)
    raw.fft$ax3.c90<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax3.c10<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
    raw.fft$ax3.kurt<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=moments::kurtosis ,by = epoch.width)
    raw.fft$ax3.skew<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=moments::skewness ,by = epoch.width)
    
    
    raw.fft$vmag.mean<-wapply(this.file$vec.mag, epoch.width, FUN=mean ,by = epoch.width)
    raw.fft$vmag.mad<-wapply(this.file$vec.mag, epoch.width, FUN=mad ,by = epoch.width)
    raw.fft$vmag.c90<-wapply(this.file$vec.mag, epoch.width, FUN=function(x){quantile(x,0.9,na.rm = TRUE)}, by=epoch.width)
    raw.fft$vmag.c10<-wapply(this.file$vec.mag, epoch.width, FUN=function(x){quantile(x,0.1,na.rm = TRUE)}, by=epoch.width)
    raw.fft$vmag.kurt<-wapply(this.file$vec.mag, epoch.width, FUN=moments::kurtosis ,by = epoch.width)
    raw.fft$vmag.skew<-wapply(this.file$vec.mag, epoch.width, FUN=moments::skewness ,by = epoch.width)
    raw.fft$ENMO<-ENMO
    
    if (isTRUE(remove.gravity)){
      raw.fft$ax1.g<-wapply(this.file$ax1.g, epoch.width, FUN=mean ,by = epoch.width)
      raw.fft$ax2.g<-wapply(this.file$ax2.g, epoch.width, FUN=mean ,by = epoch.width)
      raw.fft$ax3.g<-wapply(this.file$ax3.g, epoch.width, FUN=mean ,by = epoch.width)
    }
    
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
    
    ax2.fft<-wapply(this.file$Accelerometer.Y, epoch.width, FUN=function (x){strength.fft(x) } ,by = epoch.width)
    ax2.fft<-t(ax2.fft)
    ax2.fft<-as.data.frame(ax2.fft)
    names(ax2.fft)<-seq(1,epoch.width,1)
    raw.fft$ax2.fft.mean<-apply(ax2.fft,1,mean)
    
    ax3.fft<-wapply(this.file$Accelerometer.Z, epoch.width, FUN=function (x){strength.fft(x)} ,by = epoch.width)
    ax3.fft<-t(ax3.fft)
    ax3.fft<-as.data.frame(ax3.fft)
    names(ax3.fft)<-seq(1,epoch.width,1)
    raw.fft$ax3.fft.mean<-apply(ax3.fft,1,mean)
    
    vmag.fft<-wapply(this.file$vec.mag, epoch.width, FUN=function (x){strength.fft(x)} ,by = epoch.width)
    vmag.fft<-t(vmag.fft)
    vmag.fft<-as.data.frame(vmag.fft)
    names(vmag.fft)<-seq(1,epoch.width,1)
    raw.fft$vmag.fft.mean<-apply(ax3.fft,1,mean)
    
    
    raw.fft$cor.xy<-wapply.xy(x=this.file$Accelerometer.X,y=this.file$Accelerometer.Y,width=epoch.width,by=epoch.width,FUN=cor)
    raw.fft$cor.xz<-wapply.xy(x=this.file$Accelerometer.X,y=this.file$Accelerometer.Z,width=epoch.width,by=epoch.width,FUN=cor)
    raw.fft$cor.yz<-wapply.xy(x=this.file$Accelerometer.Y,y=this.file$Accelerometer.Z,width=epoch.width,by=epoch.width,FUN=cor)
    
    # work out how many seconds you skip fromt he start to this block
    if (i>1){
      current.sec.skip<-(start.markers[i]-1+120000)/samples.per.second
    } else{
      current.sec.skip<-(start.markers[i]-1)/samples.per.second
    }
    
    # work out the start datetime of this block
    new.start<-start.datetime+current.sec.skip
    
    # create date.time variable for this block
    raw.fft$date.time<-seq(new.start,(new.start+length(raw.fft[,1])*epoch.length)-epoch.length,epoch.length)
    raw.fft$id<-participant.id
    
    raw.fft$date.time.sec<-unclass(as.POSIXct(raw.fft$date.time))
    
    out[[i]]<-raw.fft
    
  }
  parallel::stopCluster(cl)
  
  #stick the list of data.frames into one
  out.data<-do.call(rbind, out)
  
  if (nonwear==TRUE){
    print(paste(participant.id,"Summaries calculated, calculating nonwear"))
    ggir.nonwear<-GGIR::g.getmeta(accfile,windowsizes = c(epoch.length,900,3600)
                                  ,scale = ggir.calib$scale,offset = ggir.calib$offset)

    metalong<-ggir.nonwear$metalong
    metalong$timestamp<-strptime(metalong$timestamp,format="%Y-%m-%dT%H:%M:%S")
    metalong$date.time.sec<-unclass(as.POSIXct(metalong$timestamp))

    out.data$nonwear.marker<-numeric(length(out.data[,1]))
    for (j in 1:(length(metalong[,1])-1)){
      if (metalong$nonwearscore[j]>0){
        out.data$nonwear.marker[out.data$date.time.sec>=metalong$date.time.sec[j] &
                                  out.data$date.time.sec<metalong$date.time.sec[j+1]]<-1
      }
    }

    print(paste(participant.id,"Nonwear time assessed"))
  } else{
  }
  
  return(out.data)
}
