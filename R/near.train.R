near.train<-function(dataset,trainline.psp){
  merged.data<-dataset
  ######################################
  # add the near train data to the merged dataset
  merged.data$near.train<-NA
  
  #merged.data$longitude[merged.data$latitude==90]<-NA
  #merged.data$speed[merged.data$latitude==90]<-NA
  #merged.data$latitude[merged.data$latitude==90]<-NA
  
  #merged.data$speed[merged.data$longitude>10]<-NA
  #merged.data$latitude[merged.data$longitude>10]<-NA
  #merged.data$longitude[merged.data$longitude>10]<-NA
  
  # take a subset of the data that has valid GPS data
  # and turn it into a SpatialPointsDataFrame, with projection information
  only.gps<-subset(merged.data,!is.na(speed))
  merged.data$easting<-NA
  merged.data$northing<-NA
  if (length(only.gps[,1])>0){
    gps.spatial<-SpatialPointsDataFrame(cbind(only.gps$longitude,only.gps$latitude)
                                        ,data=only.gps,proj4string = CRS("+proj=longlat +datum=WGS84"))
    
    
    # convert gps.spatial to have the same projection as the train.lines data
    gps.spatial<-spTransform(gps.spatial,CRS(proj4string(train.lines)))
    
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
