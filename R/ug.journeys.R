#' @title Underground journey identifier
#' @description Checks whether gaps in data start and end close to tube stations. If they do
#' and the gap lasts less than two hours, the missing data is marked as a missed tube journey
#' @param dataset 
#' the dataset to check for missed tube journeys
#' @param station.ppp 
#' a dataset of underground stations to measure distance to, in the spatstat format .ppp
#' @details
#' Firstly looks for gaps in data over 120 seconds and under 2 hours, then, for each start and end point 
#' of those gaps, it measures distance to the nearest underground station. If both start and end point are within
#' 200m of an underground station, and it is not the same underground station, the missing data is marked as
#' a potential underground journey, with  a length of the gap in the data, in seconds
#'
#' @export

##################
ug.journeys-function(dataset, station.ppp){
  bl<-dataset
  
  bl$ug.marker<-0
  
  bl.owin<-spatstat::owin(xrange=c(min(bl$easting),max(bl$easting)),yrange=c(min(bl$northing),max(bl$northing)))
  bl.ppp<-spatstat::ppp(bl$easting,bl$northing,window = bl.owin)
  
  bl$dist.station<-nncross(bl.ppp,station.ppp)[,1]
  
  for (j in 2:length(bl[,1])){
    time.gap<-0
    if (bl$time.since.last[j]>=120 & !is.na(bl$time.since.last[j]) & bl$time.since.last[j]<7200){
      time.gap<-1
    }
    
    potential.end<-0
    if (bl$bus.pred[j]=="train"){
      potential.end<-1
    }
    if (bl$dist.station[j]<=200){
      potential.end<-1
    }
    
    potential.start<-0
    if (bl$bus.pred[j-1]=="train"){
      potential.start<-1
    }
    if (bl$dist.station[j-1]<=200){
      potential.start<-1
    }
    
    away.frm.last<-0
    dist.since.last<-sqrt((bl$easting[j]-bl$easting[j-1])^2+(bl$northing[j]-bl$northing[j-1])^2)
    if(dist.since.last>200){
      away.frm.last<-1
    }
    
    if (time.gap==1 & potential.start==1 & potential.end==1 & away.frm.last==1){
      bl$ug.marker[j]<-1
    }
  }
  return(bl$ug.marker)
}