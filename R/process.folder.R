process.folder<-function(folder_location){

  # load input options file
  input.options<-read.csv(paste(folder_location,"/input_options.csv",sep=""),stringsAsFactors = FALSE)[,1:2]
  head(input.options)
  
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
  
  # extract train relevant variables
  train<-as.logical(input.options$value[input.options$name=="train"][1])
  train.name<-as.character(input.options$value[input.options$name=="train.name"][1])
  train.type<-as.character(input.options$value[input.options$name=="train.type"][1])
  
  # crrently assuming shapefile
  if (isTRUE(train)){
    train.data<-rgdal::readOGR(dsn = paste(folder_location,"/train line data")
                               , layer = train.name
                               , driver = "ESRI Shapefile")
    train.psp<-spatstat::as.psp(train.data)
  }
  
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
      # check if they want the gps data cleaned, then clean it
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
        write.csv(data.loss, paste(folder_location,"/data loss/",input.data$id[1],".csv",sep=""))
      }
      
      #check if they want distance to trian lines done, then do it
      if (isTRUE(train)){
        input.data<-near.train(dataset = input.data
                                , trainline.psp = trainline.psp
                                , trainline.p4s = sp::CRS(sp::proj4string(train.data)))
      }
      
      if (isTRUE(travel.mode)){
        # distance to the next minute away
        input.data$dist.next.min<-distance.moved(dataset = input.data,
                                                  last=FALSE,
                                                  time.window = 60,
                                                  epoch.length = 10)
        
        # distance to the last minute
        input.data$dist.last.min<-distance.moved(dataset = input.data,
                                                  last=TRUE,
                                                  time.window = 60,
                                                  epoch.length = 10)
        
        # calculate rolling averages
        input.data<-rollav.calc(dataset=input.data)
        
        # predict travel mode, 5 modes
        input.data$pred.mode<-predict(fitted.fullmod,newdata = as.matrix(pred.data(input.data)),type="response")
        input.data$pred.mode<-factor(input.data$pred.mode, labels=c("cycle","stat","train","vehicle","walk"))  
        
        # predict travel mode, 6 modes
        input.data$bus.pred<-predict(fitted.busmod,newdata = as.matrix(pred.data(input.data)),type="response")
        input.data$bus.pred<-factor(input.data$bus.pred, labels=c("bus","cycle","stat","train","vehicle","walk")) 
      }
      
      # write processed data file
      write.csv(input.data, paste(folder_location,"/output/processed files/",input.data$id[1],".csv",sep=""))
    }
    
    
  }
}

