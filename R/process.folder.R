#' @title Process a folder containing accelerometer and GPS data
#' @description writes a folder worth of .csv files of processed data
#' @param folder_location the location fo the folders, the rest of the inputs have to ve edited in the input_options.csv in that folder
#' @details
#' Currently only tested with Qstarz GPS device files. If you need other types contect the author
#' , they can be included with ease
#'
#' @export

process.folder<-function(folder_location){

  # load input options file
  input.options<-read.csv(paste(folder_location,"/input_options.csv",sep=""),stringsAsFactors = FALSE)[,2:3]
  
  names(input.options)<-c("name","value")
  
  #list of accelerometer files
  accelerometer.files<-dir(paste(folder_location,"/accelerometer data",sep=""), full.names=TRUE)
  
  # extract accelerometer relevant commands
  acc.prefix<-as.character(input.options$value[input.options$name=="filename.prefix"][1])
  acc.suffix<-as.character(input.options$value[input.options$name=="filename.suffix"][1])
  cutoff.method<-as.numeric(input.options$value[input.options$name=="cutoff.method"][1])
  epoch.length<-as.numeric(input.options$value[input.options$name=="epoch.length"][1])
  acc.model<-as.character(input.options$value[input.options$name=="acc.model"][1])
  raw<-as.logical(input.options$value[input.options$name=="raw"][1])
  samples.per.second<-as.numeric(input.options$value[input.options$name=="samples.per.second"][1])
  nonwear<-as.logical(input.options$value[input.options$name=="nonwear"][1])
  nonwear.method<-as.character(input.options$value[input.options$name=="nonwear.method"][1])
  
  #extract id list from accelerometer files
  id.list<-gsub(".csv","",dir(paste(folder_location,"/accelerometer data",sep="")))
  id.list<-substr(id.list,start=1+nchar(acc.prefix),stop=nchar(id.list)-nchar(acc.suffix))
  
  # extract gps file list
  gps.files<-dir(paste(folder_location,"/gps data",sep=""))
  
  # extract gps relevant inputs
  gps.prefix<-as.character(input.options$value[input.options$name=="gps.prefix"][1])
  gps.suffix<-as.character(input.options$value[input.options$name=="gps.suffix"][1])
  british.time<-as.logical(input.options$value[input.options$name=="british.time"][1])
  UTC.offset<-as.numeric(input.options$value[input.options$name=="UTC.offset"][1])
  clean.gps<-as.logical(input.options$value[input.options$name=="clean.gps"][1])
  speed.cutoff<-as.numeric(input.options$value[input.options$name=="speed.cutoff"][1])
  hdop.cutoff<-as.numeric(input.options$value[input.options$name=="hdop.cutoff"][1])
  neighbour.number<-as.numeric(input.options$value[input.options$name=="neighbour.number"][1])
  neighbour.window<-as.numeric(input.options$value[input.options$name=="neighbour.window"][1])
  ### EDITED 29 01 19
  country<-as.character(input.options$value[input.options$name=="country"][1])
  
  # check if files are to be cleared and clear the,
  clear.files<-as.logical(input.options$value[input.options$name=="clear.files"][1])
  
  if (isTRUE(clear.files)){
    do.call(file.remove, list(dir(paste(folder_location,"/output/processed files",sep=""), full.names = TRUE)))
    do.call(file.remove, list(dir(paste(folder_location,"/output/data loss",sep=""), full.names = TRUE)))
  }
  
  # extract train relevant variables
  train<-as.logical(input.options$value[input.options$name=="train"][1])
  train.name<-as.character(input.options$value[input.options$name=="train.name"][1])

  
  # currently assuming shapefile
  if (isTRUE(train)){
    require(maptools)
    train.data<-rgdal::readOGR(dsn = paste(folder_location,"/train line data",sep="")
                               , layer = train.name)
    train.psp<-spatstat::as.psp(train.data)
  }
  

  travel.mode<-as.logical(input.options$value[input.options$name=="travel.mode"][1])
  
  # loop through accelerometer files and process them
  for (i in 1:length(accelerometer.files)){
    
    if (!file.exists(paste(folder_location,"/gps data/",gps.prefix,id.list[i],gps.suffix,".csv",sep=""))){
      print(paste("There's no gps data for ",id.list[i],", so no processing will be done"))
    } else{
      acc.data<-process.acc(accfile = accelerometer.files[i]
                            , participant.id = id.list[i]
                            , cutoff.method = cutoff.method
                            , epoch.length = epoch.length
                            , acc.model = acc.model
                            , raw = raw
                            , samples.per.second = samples.per.second
                            , nonwear = nonwear)
      
      input.data<-gps.acc.merge(acc.data = acc.data
                                 , gpsfile = paste(folder_location,"/gps data/",gps.prefix,id.list[i],gps.suffix,".csv",sep="")
                                 , participant.id = id.list[i]
                                 , epoch.length = epoch.length
                                 , british.time = british.time
                                 , UTC.offset = UTC.offset
                                  )
      
      
      # # check if they want the gps data cleaned, then clean it
      if (isTRUE(clean.gps)){
        input.data<-gps.cleaner(speed.cutoff = speed.cutoff
                                 , hdop.cutoff = hdop.cutoff
                                 , neighbour.number = neighbour.number
                                 , neighbour.window = neighbour.window
                                 , epoch.length = epoch.length
                                 , dataset = input.data)

        # measure the quantities of data lost
        data.loss<-data.loss.gps(speed.cutoff = speed.cutoff
                                 , hdop.cutoff = hdop.cutoff
                                 , neighbour.number = neighbour.number
                                 , neighbour.window = neighbour.window
                                 , epoch.length = epoch.length
                                 , dataset = input.data)
        #write the data lost to file
        write.csv(data.loss, paste(folder_location,"/output/data loss/",input.data$id[1],".csv",sep=""))
        
      }
      
      # convert to eastings/northings
      input.data$easting<-NA
      input.data$northing<-NA
      gps.data<-subset(input.data, !is.na(longitude))
      
      if (country=="UK" & length(gps.data[,1])>0){
        spat<-sp::SpatialPointsDataFrame(cbind(gps.data$longitude, gps.data$latitude),
                                         data=gps.data, proj4string = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
        spat<-sp::spTransform(spat, CRSobj = sp::CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs "))
        input.data$easting[!is.na(input.data$latitude)]<-sp::coordinates(spat)[,1]
        input.data$northing[!is.na(input.data$latitude)]<-sp::coordinates(spat)[,2]
      } 
      

      #check if they want distance to train lines done, then do it
      if (isTRUE(train)){
        input.data<-near.train(dataset = input.data
                                , trainline.psp = train.psp
                                , trainline.p4s = sp::proj4string(train.data)
                                , country = country)
      } else{
        input.data$near.train<-NA
      }

      if (isTRUE(travel.mode)){

        # calculate rolling averages
        input.data<-rollav.calc(dataset=input.data)

        # predict travel mode, 5 modes
        input.data$pred.mode<-predict(fitted.fullmod,newdata = as.matrix(pred.data(input.data)),type="response")
        sub.levels<-as.numeric(levels(factor(input.data$pred.mode)))
        input.data$pred.mode<-factor(input.data$pred.mode, labels=c("cycle","stat","train","vehicle","walk")[sub.levels])

        # predict travel mode, 6 modes
        input.data$bus.pred<-predict(fitted.busmod,newdata = as.matrix(pred.data(input.data)),type="response")
        sub.levels<-as.numeric(levels(factor(input.data$bus.pred)))
        input.data$bus.pred<-factor(input.data$bus.pred, labels=c("bus","cycle","stat","train","vehicle","walk")[sub.levels])
      }
      

      # write processed data file
      write.csv(input.data, paste(folder_location,"/output/processed files/",input.data$id[1],".csv",sep=""))
    }
    
    
  }
}