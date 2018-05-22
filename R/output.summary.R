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
  
  by.day<-as.logical(out.options$option[out.options$how.to.summarize=="by.day"][1])
  by.week<-as.logical(out.options$option[out.options$how.to.summarize=="by.week"][1])
  travel.modes<-as.logical(out.options$option[out.options$how.to.summarize=="travel.modes"][1])
  six.modes<-as.logical(out.options$option[out.options$how.to.summarize=="six.modes"][1])
  activity<-as.logical(out.options$option[out.options$how.to.summarize=="activity"][1])
  
  if (isTRUE(by.day)){
    day.data<-data.frame()
  }
  
  if (isTRUE(by.week)){
    week.data<-data.frame()
  }

  
  for (i in 1:length(processed.files)){
    
    input.data<-read.csv(processed.files[i])
    
    if (isTRUE(by.day)){
      if (length(input.data[,1])>1){
        if (length(levels(factor(input.data$day)))>1){
          out.data<-aggregate(id~day,data=input.data, FUN=function(x) x[2])
          
          ####################need to measure day order
          #out.data$day.order<-aggregate(day.order~day,data=input.data, FUN=function(x) x[1])[,2]
          out.data$date<-aggregate(date.time~day,data=input.data, FUN=function(x) substr(x[2],start=1,stop=10))[,2]
          out.data$month<-lubridate::month(strptime(out.data$date, format="%Y-%m-%d"), label=TRUE)
          
          out.data$total.epochs<-aggregate(pred.mode~day,data=input.data, FUN=length)[,2]
          
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
          
          if (!is.null(input.data$ug.length)){
            out.data$ug<-aggregate(ug.length~day, data=input.data, FUN=sum)[,2]/10
            out.data$total.epochs<-out.data$total.epochs+out.data$ug
          } else{
            out.data$ug<-NA
          }
          
        } else{
          day<-input.data$day[1]
          id<-input.data$id[1]
          date<-substr(input.data$date.time[1],start=1,stop=10)
          month<-lubridate::month(strptime(date, format="%Y-%m-%d"),label=TRUE)
          total.epochs<-length(input.data[,1])
          walk<-length(input.data[,1][input.data$pred.mode=="walk" &!is.na(input.data$pred.mode)])
          cycle<-length(input.data[,1][input.data$pred.mode=="cycle" &!is.na(input.data$pred.mode)])
          vehicle<-length(input.data[,1][input.data$pred.mode=="vehicle" &!is.na(input.data$pred.mode)])
          train<-length(input.data[,1][input.data$pred.mode=="train" &!is.na(input.data$pred.mode)])
          stat<-length(input.data[,1][input.data$pred.mode=="stat" &!is.na(input.data$pred.mode)])
          if (!is.null(input.data$ug.length)){
            ug<-sum(input.data$ug.length)/10
            total.epochs<-total.epochs+ug
          } else{
            ug<-NA
          }
          
          out.data<-data.frame(day, id, date, month, total.epochs, walk, cycle
                               , vehicle, train, stat, ug)
          
        }
        write.csv(out.data,paste(folder_location, "/output/day files/", out.data$id[1],".csv", sep=""), row.names = FALSE)
        day.data<-rbind(day.data,out.data)
    }
    }
    
    if (isTRUE(by.week)){
      id<-input.data$id[1]
      date<-substr(input.data$date.time[1],start=1,stop=10)
      month<-lubridate::month(strptime(date, format="%Y-%m-%d"),label=TRUE)
      total.epochs<-length(input.data[,1])
      walk<-length(input.data[,1][input.data$pred.mode=="walk" &!is.na(input.data$pred.mode)])
      cycle<-length(input.data[,1][input.data$pred.mode=="cycle" &!is.na(input.data$pred.mode)])
      vehicle<-length(input.data[,1][input.data$pred.mode=="vehicle" &!is.na(input.data$pred.mode)])
      train<-length(input.data[,1][input.data$pred.mode=="train" &!is.na(input.data$pred.mode)])
      stat<-length(input.data[,1][input.data$pred.mode=="stat" &!is.na(input.data$pred.mode)])
      if (!is.null(input.data$ug.length)){
        ug<-sum(input.data$ug.length)/10
        total.epochs<-total.epochs+ug
      } else{
        ug<-NA
      }
      
      out.data<-data.frame(id, date, month, total.epochs, walk, cycle
                           , vehicle, train, stat, ug)
      
      write.csv(out.data, paste(folder_location, "/output/week files/", out.data$id[1],".csv", sep=""), row.names = FALSE)
      week.data<-rbind(week.data,out.data)
    }
  }
  
  if (isTRUE(by.day)){
    write.csv(day.data,paste(folder_location, "/output/summary files/", "day_data.csv", sep=""), row.names = FALSE)
  }
  
  if (isTRUE(by.week)){
    write.csv(week.data,paste(folder_location, "/output/summary files/", "week_data.csv", sep=""), row.names = FALSE)
  }
  
}