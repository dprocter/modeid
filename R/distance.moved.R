#' @export
#' @title Distance moved
#' @description a function that measures distance moved between points separated by a time window
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
