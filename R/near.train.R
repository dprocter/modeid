#' @title Distance from all points to the nearest train line
#' @description Measures euclidean distance from each point to the nearest trainline
#' @param dataset a merged gps/accelerometer dataset
#' @param trainline.psp a \code{psp} (line segement pattern from the package \link[package]{spatstat})
#' of train lines
#' @param trainline.p4s the \code{proj4string} of the trainline
#' @details We assume you have processed accelerometer data using the \code{\link{process.acc}}
#' and \code{\link{gps.acc.merge}}. This function then takes train line data, in \code{psp} format, 
#' and uses the \code{nncross} function from the \link[package]{spatstat} package to measure distance from 
#' each point to the nearest train line.
#' 
#' To find out the \code{proj4string} of the trainline data, use package \link[package]{sp} function 
#' \code{proj4string} on a \code{SpatialLinesDataFrame} of train data
#' 
#' @export

near.train<-function(dataset, trainline.psp, trainline.p4s){
  merged.data<-dataset
  ######################################
  # add the near train data to the merged dataset
  merged.data$near.train<-NA
  
  # take a subset of the data that has valid GPS data
  # and turn it into a SpatialPointsDataFrame, with projection information
  only.gps<-subset(merged.data,!is.na(speed))
  merged.data$easting<-NA
  merged.data$northing<-NA
  if (length(only.gps[,1])>0){
    gps.spatial<-SpatialPointsDataFrame(cbind(only.gps$longitude,only.gps$latitude)
                                        ,data=only.gps,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    
    # convert gps.spatial to have the same projection as the train.lines data
    gps.spatial<-spTransform(gps.spatial,CRS(trainline.p4s))
    
    #creates a bounding box around the points (ppp's need these)
    gps.win<-owin(xrange=c(min(coordinates(gps.spatial)[,1]-1000),max(coordinates(gps.spatial)[,1])+1000)
                  ,yrange=c(min(coordinates(gps.spatial)[,2]-1000),max(coordinates(gps.spatial)[,2]+1000)))
    # turns the gps data into a sptial point pattern
    gps.ppp<-as.ppp(coordinates(gps.spatial),W=gps.win)
    
    
    # the nncross function from spatstat gives you sitance from each point to the nearest line
    merged.data$near.train[!is.na(merged.data$speed)]<-nncross(gps.ppp,trainline.psp)[,1]
    
    merged.data$easting[!is.na(merged.data$longitude)]<-coordinates(gps.spatial)[,1]
    merged.data$northing[!is.na(merged.data$longitude)]<-coordinates(gps.spatial)[,2]
  }
  
  return(merged.data)
}
