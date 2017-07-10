# assumes your speed variable is called speed, and your HDOP variable is called hdop, if they aren't rename them fist
#speed cutoff, neighbour.window and epoch length in seconds
gps.cleaner<-function(speed.cutoff, hdop.cutoff, neighbour.number, neighbour.window, epoch.length, dataset){
  this.data<-dataset
  if (is.null(dataset$speed)){
    print("No speed variable, so I assume no gps data")
  } else{

    this.data$gps.counter<-numeric(length(this.data[,1])) #creates gps counter variable
    this.data$gps.counter[!is.na(this.data$speed)]<-1 # sets gps counter to 1 if there is valid gps data, which we assume ther eis if there is a speed variable
    this.data$rollcount<-zoo::rollsum(this.data$gps.counter,(neighbour.window/epoch.length),align="center",na.pad=T) # counts the number of valid gps points within 5 minutes
    this.data$remove.me<-numeric(length(this.data[,1])) # creates a variable that marks points for removal
    # marks points for removal if they have 2 or less neighbours within 5 minutes
    this.data$remove.me[this.data$rollcount<=neighbour.number & !is.na(this.data$speed)]<-1
    # marks points for removal if they display speed greater than the cutoff
    this.data$remove.me[this.data$speed>=speed.cutoff & !is.na(this.data$speed)]<-1
    # marks points for removal if they display HDOP greater than the cutoff
    this.data$remove.me[this.data$hdop>hdop.cutoff & !is.na(this.data$speed)]<-1

    # effectively removes all marked points by setting their gps values as NA
    this.data$easting[this.data$remove.me==1]<-NA
    this.data$northing[this.data$remove.me==1]<-NA
    this.data$sumsnr[this.data$remove.me==1]<-NA
    this.data$pdop[this.data$remove.me==1]<-NA
    this.data$hdop[this.data$remove.me==1]<-NA
    this.data$vdop[this.data$remove.me==1]<-NA
    this.data$speed[this.data$remove.me==1]<-NA

    this.data<-subset(this.data,select=-c(gps.counter,rollcount,remove.me))
    return(this.data)
  }
}
