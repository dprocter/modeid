#I am assuming this dataset has been merged from accelerometer and GPS data using the gps.acc.merge function
#If it hasn't, the accelerometer axes should be labelled "Axis1", "Axis2" etc...
#It will work for 1, 2 or 3 axes
#epoch.length in seconds, window length and interruption length in minutes
acc.nonwear<-function(dataset,epoch.length,window.length,interruption.length){
  merged.data<-dataset

  merged.data$non.zero.counter<-numeric(length(merged.data[,1]))
  merged.data$zero.counter<-numeric(length(merged.data[,1]))

  if (is.null(merged.data$Axis1)){
    print("No recognised accelerometer variable, check labelling")
  } else{

    merged.data$non.zero.counter[merged.data$Axis1>0]<-1

    if (!is.null(merged.data$Axis2)){
      merged.data$non.zero.counter[merged.data$Axis2>0]<-1
    }

    if (!is.null(merged.data$Axis3)){
      merged.data$non.zero.counter[merged.data$Axis3>0]<-1
    }

    merged.data$zero.counter[merged.data$non.zero.counter==0]<-1

    # counting up the zeros from both left and right ensures the full hour is covered
    merged.data$window.zeros<-zoo::rollapply(merged.data$zero.counter,
                                             (window.length*(60/epoch.length)),
                                             align="right",partial=TRUE,FUN=sum)
    merged.data$window.zeros2<-zoo::rollapply(merged.data$zero.counter,
                                              (window.length*(60/epoch.length)),
                                              align="left",partial=TRUE,FUN=sum)

    epoch.cutoff<-(window.length*(60/epoch.length))-(interruption.length*(60/epoch.length))
    merged.data$remove<-numeric(length(merged.data[,1]))
    # mark a remove variable, which marks when there are more 0's in an hour than the cutoff
    merged.data$remove[merged.data$window.zeros >= epoch.cutoff]<-1
    merged.data$remove[merged.data$window.zeros2 >= epoch.cutoff]<-1

    merged.data$Axis1[merged.data$remove==1]<-NA

    if (!is.null(merged.data$Axis2)){
      merged.data$Axis2[merged.data$remove==1]<-NA
    }

    if (!is.null(merged.data$Axis3)){
      merged.data$Axis3[merged.data$remove==1]<-NA
    }

    merged.data<-subset(merged.data,select=-c(non.zero.counter,zero.counter,window.zeros,window.zeros2,remove))

    return(merged.data)
  }
}
