#' @export
#' @title Distance moved
#' @description a function that measures distance moved between points separated by a time window
#'
#' @param dataset
#' The dataset you want to apply this to, we assume it has been created by processing file susing this package
#'
#' @param last
#' whether you want the time window before (last=TRUE) or the following time window (last=FALSE)
#'
#' @param time.window
#' The window across which to measure distance in seconds
#'
#' @param epoch.length
#' Th eepoch.length you are working with, in seconds
#'
#' @details
#' Given a dataset as input, this will output a variable of the same length as the datasets' variables.
#' This will contain the distance of each point to the point the specified distance away.
#'


distance.moved<-function(dataset, last, time.window, epoch.length){
  this.data<-dataset
  epochs.shifted<-time.window/epoch.length

  if (isTRUE(last)){
    this.data$east.shift<-NA
    this.data$east.shift[1:(length(this.data[,1])-epochs.shifted)]<-this.data$easting[
      (epochs.shifted+1):length(this.data[,1])]
    this.data$north.shift<-NA
    this.data$north.shift[1:(length(this.data[,1])-epochs.shifted)]<-this.data$northing[
      (epochs.shifted+1):length(this.data[,1])]
  } else{
    this.data$east.shift<-NA
    this.data$east.shift[(epochs.shifted+1):length(this.data[,1])]<-this.data$easting[
      1:(length(this.data[,1])-epochs.shifted)]
    this.data$north.shift<-NA
    this.data$north.shift[(epochs.shifted+1):length(this.data[,1])]<-this.data$northing[
      1:(length(this.data[,1])-epochs.shifted)]
  }

  dist.moved<-sqrt(
    (this.data$easting-this.data$east.shift)^2+(this.data$northing-this.data$north.shift)^2)

  return(dist.moved)
}
