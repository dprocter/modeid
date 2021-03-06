#' @export
#' @title Data loss due to GPS cleaning
#' @description
#' Returns a two column data.frame, with the data amount lost at each level
#' of data cleaning, in epochs
#' @param speed.cutoff
#' The cut-off above which speed is deemed unreliable, in the same units as your speed variable
#' @param hdop.cutoff
#' The cut-off above which your horizontal dilution of precision is deemed unreliable
#' @param neighbour.number
#' an integer: if you are removing isolated points, how few neighbours counts as isolated
#' @param neighbour.window
#' The width of a window in which to look for neighbours to define whether a point is
#' isolated, in seconds
#' @param dataset
#' The dataset you need cleaned
#' @details
#' Set up exactly the same as \code{\link{gps.cleaner}}, this function
#' tell you how much data you lose from cleaning the data with your given rules
#'

#for a file, returns two columns tellin you how many epochs you lost due to data cleaning
#speed cutoff, neighbour.window and epoch length in seconds
data.loss.gps<-function(speed.cutoff, hdop.cutoff, neighbour.number, neighbour.window, epoch.length, dataset){
  this.data<-dataset

  this.data$gps.counter<-numeric(length(this.data[,1]))
  #marks whether there is GPS data
  this.data$gps.counter[!is.na(this.data$speed)]<-1
  #counts the number of gps points within a 5 minute window, centred on the point. NAs the rest
  this.data$rollcount<-zoo::rollapply(this.data$gps.counter,(neighbour.window/epoch.length),align="center",partial=TRUE,fill=NA,FUN=sum)

  all.data<-length(this.data[,1]) # total size of merged data set
  gps.data<-length(this.data[,1][!is.na(this.data$speed)]) # total size of dataset where gps is worn
  gps.no.neighbours<-length(this.data[,1][this.data$rollcount>neighbour.number & !is.na(this.data$speed)]) # total size of dataset with gps minus those with no neighbours
  highspeed<-length(this.data[,1][this.data$rollcount>neighbour.number & this.data$speed<speed.cutoff
                                  & !is.na(this.data$speed)])              # and is going less than 160kph
  hdop<-length(this.data[,1][this.data$rollcount>neighbour.number & this.data$speed<speed.cutoff
                             & !is.na(this.data$speed) & this.data$hdop<hdop.cutoff])                            # 160kph and hdop 5 or under

  labels<-c("total.dataset.size","invalid.gps.data","no.neigbours","excess.speed","poor.signal")
  data.amounts<-c(all.data,gps.data,gps.no.neighbours
                  ,highspeed,hdop)
  data.removed<-c(0,all.data-gps.data, gps.data-gps.no.neighbours
                  ,gps.no.neighbours-highspeed,highspeed-hdop)
  id<-c(this.data$id[1],this.data$id[1],this.data$id[1],this.data$id[1],this.data$id[1])

  return(data.frame(id,labels,data.amounts,data.removed))
}

