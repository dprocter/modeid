#' @title Create summaries of processed files
#' @description summarises a folder worth of processed files by day and week
#' @param folder_location the location fo the folders, the rest of the inputs have to be edited in 
#' the output_options.csv in that folder
#' @details
#' 
#'
#' @export
output.summary<-function(folder_location){
  processed.files<-dir(paste(folder_location, "/output/processed files",sep=""),full.names = TRUE)
  
  out.options<-read.csv(paste(folder_location,"/output_options.csv", sep=""))
  
  # by hour/day/week
  by.day<-as.logical(out.options$option[out.options$how.to.summarize=="by.day"][1])
  by.week<-as.logical(out.options$option[out.options$how.to.summarize=="by.week"][1])
  # travel mode options
  travel.modes<-as.logical(out.options$option[out.options$how.to.summarize=="travel.modes"][1])
  six.modes<-as.logical(out.options$option[out.options$how.to.summarize=="six.modes"][1])
  
  # underground missed j identification
  underground<-as.logical(out.options$option[out.options$how.to.summarize=="underground"][1])
  station.name<-as.character(out.options$option[out.options$how.to.summarize=="station.name"][1])
  
  # activity options
  activity<-as.logical(out.options$option[out.options$how.to.summarize=="activity"][1])
  cut.points<-as.character(out.options$option[out.options$how.to.summarize=="act.cut.points"][1])
  shapefile<-as.logical(out.options$option[out.options$how.to.summarize=="write.shapefile"][1])
  
  # admin
  clear.files<-as.logical(out.options$option[out.options$how.to.summarize=="clear.files"][1])
  

  
  if (isTRUE(clear.files)){
      do.call(file.remove, list(dir(paste(folder_location,"/output/day files",sep=""), full.names = TRUE)))
      do.call(file.remove, list(dir(paste(folder_location,"/output/shapefiles",sep=""), full.names = TRUE)))
      do.call(file.remove, list(dir(paste(folder_location,"/output/summary files",sep=""), full.names = TRUE)))
      do.call(file.remove, list(dir(paste(folder_location,"/output/week files",sep=""), full.names = TRUE)))
  }

  
  if (isTRUE(by.day)){
    day.data<-data.frame()
  }
  
  if (isTRUE(by.week)){
    week.data<-data.frame()
  }
  
  if (isTRUE(underground)){
    station.data<-rgdal::readOGR(dsn = paste(folder_location,"/station data",sep="")
                                 , layer = station.name)
    station.ppp<-spatstat::as.ppp(station.data)
  }
  

  
  for (i in 1:length(processed.files)){
    
    input.data<-read.csv(processed.files[i])
    input.data<-subset(input.data,!is.na(latitude))
    input.data$dt.strp<-strptime(input.data$date.time, format="%Y-%m-%d %H:%M:%S")
    input.data$month<-lubridate::month(input.data$dt.strp, label=TRUE)
    
    if (length(input.data[,1])>1){
    
    if (isTRUE(underground)){
      input.data$time.since.last<-time.since.last(input.data$date.time, "%Y-%m-%d %H:%M:%S")
      input.data$ug.marker<-ug.journeys(dataset=input.data, station.ppp=station.ppp)
      input.data$ug.length<-input.data$ug.marker*input.data$time.since.last
    }
      
    if (isTRUE(activity)){
      if (cut.points=="Freedson"){
        input.data$sed.marker<-0
        input.data$sed.marker[input.data$Axis1<=16]<-1
        input.data$light.marker<-0
        input.data$light.marker[input.data$Axis1>16 & input.data$Axis1<=325]<-1
        input.data$mvpa.marker<-0
        input.data$mvpa.marker[input.data$Axis1>325]<-1
      }
      
      if (cut.points=="Evenson"){
        input.data$sed.marker<-0
        input.data$sed.marker[input.data$Axis1<=16]<-1
        input.data$light.marker<-0
        input.data$light.marker[input.data$Axis1>16 & input.data$Axis1<=382]<-1
        input.data$mvpa.marker<-0
        input.data$mvpa.marker[input.data$Axis1>382]<-1
      }
      
      
    }
    
    
    
      if (isTRUE(by.day)){
        if (length(levels(factor(input.data$day)))>1){
          out.data<-aggregate(id~day,data=input.data, FUN=function(x) x[2])
          
          out.data$day.order<-aggregate(day.order~day,data=input.data, FUN=function(x) x[1])[,2]
          out.data$date<-aggregate(date.time~day,data=input.data, FUN=function(x) substr(x[2],start=1,stop=10))[,2]
          out.data$month<-aggregate(month~day,data=input.data, FUN=function(x) x[1])[,2]
          
          out.data$total.epochs<-aggregate(id~day,data=input.data, FUN=length)[,2]
          
          if (isTRUE(travel.modes)){
            input.data$marker<-0
            input.data$marker[input.data$pred.mode=="walk" & !is.na(input.data$pred.mode)]<-1
            out.data$walk<-aggregate(marker~day,data=input.data, FUN=sum)[,2]
            
            input.data$marker<-0
            input.data$marker[input.data$pred.mode=="cycle" & !is.na(input.data$pred.mode)]<-1
            out.data$cycle<-aggregate(marker~day,data=input.data, FUN=sum)[,2]
            
            input.data$marker<-0
            input.data$marker[input.data$pred.mode=="vehicle" & !is.na(input.data$pred.mode)]<-1
            out.data$vehicle<-aggregate(marker~day,data=input.data, FUN=sum)[,2]
            
            input.data$marker<-0
            input.data$marker[input.data$pred.mode=="train" & !is.na(input.data$pred.mode)]<-1
            out.data$train<-aggregate(marker~day,data=input.data, FUN=sum)[,2]
            
            input.data$marker<-0
            input.data$marker[input.data$pred.mode=="stat" & !is.na(input.data$pred.mode)]<-1
            out.data$stat<-aggregate(marker~day,data=input.data, FUN=sum)[,2]
            
          }
          
          if (!is.null(input.data$ug.length)){
            out.data$ug<-aggregate(ug.length~day, data=input.data, FUN=function(x) sum(na.omit(x)))[,2]/10
            out.data$total.epochs<-out.data$total.epochs+out.data$ug
          } 
          if (isTRUE(activity) & cut.points!="Raw"){
            out.data$sed<-aggregate(sed.marker~day, data=input.data, FUN=sum)[,2]
            out.data$light<-aggregate(light.marker~day, data=input.data, FUN=sum)[,2]
            out.data$mvpa<-aggregate(mvpa.marker~day, data=input.data, FUN=sum)[,2]
            out.data$mean.cpm<-aggregate(Axis1~day, data=input.data, FUN=mean)[,2]
          }
          if (isTRUE(activity) & cut.points=="Raw"){
            out.data$mean.ENMO<-aggregate(ENMO~day, data=input.data, FUN=mean)[,2]
          }
          
        } else{
          
          day<-input.data$day[1]
          out.data<-data.frame(day)
          out.data$day.order<-input.data$day.order[1]
          out.data$id<-input.data$id[1]
          out.data$date<-substr(input.data$date.time[1],start=1,stop=10)
          out.data$month<-input.data$month[1]
          out.data$total.epochs<-length(input.data[,1])
          if (isTRUE(travel.modes)){
            out.data$walk<-length(input.data[,1][input.data$pred.mode=="walk" &!is.na(input.data$pred.mode)])
            out.data$cycle<-length(input.data[,1][input.data$pred.mode=="cycle" &!is.na(input.data$pred.mode)])
            out.data$vehicle<-length(input.data[,1][input.data$pred.mode=="vehicle" &!is.na(input.data$pred.mode)])
            out.data$train<-length(input.data[,1][input.data$pred.mode=="train" &!is.na(input.data$pred.mode)])
            out.data$stat<-length(input.data[,1][input.data$pred.mode=="stat" &!is.na(input.data$pred.mode)])
          }
          
          if (!is.null(input.data$ug.length)){
            out.data$ug<-sum(na.omit(input.data$ug.length))/10
            out.data$total.epochs<-out.data$total.epochs+out.data$ug
          } 
          if (isTRUE(activity) & cut.points!="Raw"){
            out.data$sed<-sum(input.data$sed.marker)
            out.data$light<-sum(input.data$light.marker)
            out.data$mvpa<-sum(input.data$mvpa.marker)
            out.data$mean.cpm<-mean(input.data$Axis1)
          }
          if (isTRUE(activity) & cut.points=="Raw"){
            out.data$mean.ENMO<-mean(input.data$ENMO)
          }
          
        }
        write.csv(out.data,paste(folder_location, "/output/day files/", out.data$id[1],".csv", sep=""), row.names = FALSE)
        day.data<-rbind(day.data,out.data)
      }
      
      # error somewhere in here
      if (isTRUE(by.week)){
        
        id<-input.data$id[1]
        out.data<-data.frame(id)
        out.data$date<-substr(input.data$date.time[1],start=1,stop=10)
        out.data$month<-input.data$month[1]
        out.data$total.epochs<-length(input.data[,1])
        if (isTRUE(travel.modes)){
          out.data$walk<-length(input.data[,1][input.data$pred.mode=="walk" &!is.na(input.data$pred.mode)])
          out.data$cycle<-length(input.data[,1][input.data$pred.mode=="cycle" &!is.na(input.data$pred.mode)])
          out.data$vehicle<-length(input.data[,1][input.data$pred.mode=="vehicle" &!is.na(input.data$pred.mode)])
          out.data$train<-length(input.data[,1][input.data$pred.mode=="train" &!is.na(input.data$pred.mode)])
          out.data$stat<-length(input.data[,1][input.data$pred.mode=="stat" &!is.na(input.data$pred.mode)])
        }
        
        if (!is.null(input.data$ug.length)){
          out.data$ug<-sum(na.omit(input.data$ug.length))/10
          out.data$total.epochs<-out.data$total.epochs+out.data$ug
        }
        if (isTRUE(activity) & cut.points!="Raw"){
          out.data$sed<-sum(input.data$sed.marker)
          out.data$light<-sum(input.data$light.marker)
          out.data$mvpa<-sum(input.data$mvpa.marker)
        }
        if (isTRUE(activity) & cut.points=="Raw"){
          out.data$mean.ENMO<-mean(input.data$ENMO)
        }
        
        write.csv(out.data, paste(folder_location, "/output/week files/", out.data$id[1],".csv", sep=""), row.names = FALSE)
        week.data<-rbind(week.data,out.data)
      }
      
      if (isTRUE(shapefile)){
        input.data$month<-as.character(input.data$month)
        spat<-sp::SpatialPointsDataFrame(cbind(input.data$easting, input.data$northing),
                                         data=input.data
                                         , proj4string = sp::CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "))
        rgdal::writeOGR(spat, dsn=paste(folder_location, "/output/shapefiles",sep=""), layer=input.data$id[1], driver="ESRI Shapefile")
      }
    }
    }
    
    
  if (isTRUE(by.day)){
    write.csv(day.data,paste(folder_location, "/output/summary files/", "day_data.csv", sep=""), row.names = FALSE)
  }
  
  if (isTRUE(by.week)){
    write.csv(week.data,paste(folder_location, "/output/summary files/", "week_data.csv", sep=""), row.names = FALSE)
  }
  
  if(length(dir(paste(folder_location,"/output/data loss",sep=""), full.names = TRUE))>0){
    lost.data<-data.frame()
    lost.files<-dir(paste(folder_location,"/output/data loss",sep=""), full.names = TRUE)
    for (j in 1:length(lost.files)){
      dl<-read.csv(lost.files[j])
      lost.data<-rbind(lost.data, dl)
    }
    
    write.csv(lost.data, paste(folder_location,"/output/summary files/data_loss.csv",sep=""), row.names = FALSE)
  }
  
  }
  
  