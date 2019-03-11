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

near.train<-function(dataset, trainline.psp, trainline.p4s, country){
  require(maptools)
  
  merged.data<-dataset
  ######################################
  # add the near train data to the merged dataset
  merged.data$near.train<-NA
  
  # take a subset of the data that has valid GPS data
  # and turn it into a SpatialPointsDataFrame, with projection information
  only.gps<-subset(merged.data,!is.na(speed))
  if (length(only.gps[,1])>0 & country=="UK"){
    
    merged.data$easting<-NA
    merged.data$northing<-NA
    
    gps.spatial<-sp::SpatialPointsDataFrame(cbind(only.gps$longitude,only.gps$latitude)
                                              ,data=only.gps,proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    
    # convert gps.spatial to have the same projection as the train.lines data
    gps.spatial<-sp::spTransform(gps.spatial,CRS(trainline.p4s))
    
    #creates a bounding box around the points (ppp's need these)
    gps.win<-spatstat::owin(xrange=c(min(coordinates(gps.spatial)[,1]-1000),max(coordinates(gps.spatial)[,1])+1000)
                  ,yrange=c(min(coordinates(gps.spatial)[,2]-1000),max(coordinates(gps.spatial)[,2]+1000)))
    # turns the gps data into a sptial point pattern
    gps.ppp<-spatstat::as.ppp(coordinates(gps.spatial),W=gps.win)
    
    
    # the nncross function from spatstat gives you sitance from each point to the nearest line
    merged.data$near.train[!is.na(merged.data$speed)]<-spatstat::nncross(gps.ppp,trainline.psp)[,1]
    
    merged.data$easting[!is.na(merged.data$longitude)]<-sp::coordinates(gps.spatial)[,1]
    merged.data$northing[!is.na(merged.data$longitude)]<-sp::coordinates(gps.spatial)[,2]
  } else {
      if (length(only.gps[,1])>0 & country!="UK"){
      
      gps.spatial<-sp::SpatialPointsDataFrame(cbind(only.gps$longitude,only.gps$latitude)
                                              ,data=only.gps,proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      
      
      
      # turns the gps data into a sptial point pattern
      gps.ppp<-spatstat::as.ppp(gps.spatial)
      
      
      # the nncross function from spatstat gives you distance from each point to the nearest line
      raw_dist<-spatstat::nncross(gps.ppp,trainline.psp)[,1]
      
      # this is a simplification of finding the long/lat of both each point and the nearest point of each line
      # then measuring haversine distance. This conversion will be inaccurate for large distances, but the algorithm
      # only cares about short distances
      merged.data$near.train[!is.na(merged.data$speed)]<-asin(raw_dist)*0.016*6378100

      
    }
  }
  
  return(merged.data)
}
