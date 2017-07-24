#' @export
#' @title Cleaning of GPS data
#' @description Returns a data.frame in the same format as the input
#' but with points deemed unreliable set as NA
#' @param speed.cutoff
#' The cut-off above which speed is deemed unreliable, in the same units as your speed variable
#' @param hdop.cutoff
#' THe cut-off above which your horizontal dilution of precision is deemed unreliable
#' @param neighbour.number
#' an integer: if you are removing isolated points, how few neighbours counts as isolated
#' @param neighbour.window
#' The width of a window in which to look for neighbours to define whether a point is
#' isolated, in seconds
#' @param dataset
#' The dataset you need cleaned
#' @return
#' A data.frame in the same format as your input, but with GPS related variables set as NA
#' @details
#' Cleans unreliable points base on speed, HDOP and number of neighbours.
#' Assumes you have used the \code{\link{gps.acc.merger}} function, and your data is set
#' up in that manner. If you have not that is not a problem, but the function expects
#' variables named "longitude","latitude","speed","hdop","pdop","vdop" and "sumsnr".
#' If you want all of those things set as NA, you need the variables labelled correctly

# assumes your speed variable is called speed, and your HDOP variable is called hdop, if they aren't rename them firt
#neighbour.window and epoch length in seconds
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
    this.data$longitude[this.data$remove.me==1]<-NA
    this.data$latitude[this.data$remove.me==1]<-NA
    #this.data$easting[this.data$remove.me==1]<-NA
    #this.data$northing[this.data$remove.me==1]<-NA
    this.data$sumsnr[this.data$remove.me==1]<-NA
    this.data$pdop[this.data$remove.me==1]<-NA
    this.data$hdop[this.data$remove.me==1]<-NA
    this.data$vdop[this.data$remove.me==1]<-NA
    this.data$speed[this.data$remove.me==1]<-NA

    this.data<-subset(this.data,select=-c(gps.counter,rollcount,remove.me))
    return(this.data)
  }
}
