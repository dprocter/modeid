#' @export
#' @title Post processing: segmentation
#' @description
#' post3 identifies segements in the data, then assessed if one predicted mode
#' dominates this segement. If it does, post 3 sets the entire segement as
#' that mode.
#' @param pred.variable
#' The variable containing predictions that you want to segment
#' @param dataset
#' the dataset your prediction is contained within
#' @param window.width
#' The time gap between points that dentes a new segment, in seconds
#' @param segment.length
#' The minimum length of a segment in seconds
#' @param epoch.length
#' The length of the epoch, in seconds
#' @return
#' A variable of post processed travel mode, in the same format as as
#' \code{pred.variable}



# the third stage of post processing, identify contiguous segments and set them to the same journey mode
# segments are separated by gaps of two minutes between valid points or changed in predicted travel mode
# which are constant for two minutes
post3<-function(pred.variable, dataset, window.width, segment.length, epoch.length){
  this.data<-dataset
  this.data$pred<-pred.variable

  #make sure there is a time.since.last variable
  this.data$time.since.last<-time.since.last(this.data$date.time,format="%Y-%m-%d %H:%M:%S")

  #identifies segment end points based on time
  this.data$seg.end<-numeric(length(this.data[,1]))
  this.data$seg.end[this.data$time.since.last>=window.width]<-1
  #identifies a second end point, based on whether there is a change in predicted mode
  this.data$seg.end2<-numeric(length(this.data[,1]))

  #the mode of the next point
  this.data$pred.next<-this.data$pred
  this.data$pred.next[length(this.data$pred.next)]<-NA
  this.data$pred.next[1:length(this.data$pred.next)-1]<-this.data$pred[2:length(this.data$pred)]

  # if the next point doesn't show the same mode as the current point, mark a change in segment
  this.data$seg.end2[!is.na(this.data$pred) & !is.na(this.data$pred.next)
                     & this.data$pred!=this.data$pred.next]<-1

  # a loop that marks what segment you are on, dependent on the previous point and whether
  #the current point is a jounrey end
  this.data$segment<-numeric(length(this.data[,1]))+1
  for (q in 2:length(this.data[,1])){
    if (this.data$seg.end[q]==1 | this.data$seg.end2[q]==1){
      this.data$segment[q]<-this.data$segment[q-1]+1
    } else{
      this.data$segment[q]<-this.data$segment[q-1]
    }

  }

  # a refinement of segment, which says: if the given segment is under 2 minutes long and within 2 minutes of the
  # previous journey, combine it with the previous journey
  this.data$segment2<-numeric(length(this.data[,1]))+1
  if(max(this.data$segment>1)){
    for (j.num in 2:max(this.data$segment)){
      prev.j<-subset(this.data,this.data$segment==j.num-1)
      this.j<-subset(this.data,this.data$segment==j.num)
      if (length(this.j[,1])<=(segment.length/epoch.length) & this.j$time.since.last[1]<=segment.length){
        this.data$segment2[this.data$segment==j.num]<-prev.j$segment2[1]
      } else{
        this.data$segment2[this.data$segment==j.num]<-this.j$segment[1]
      }
    }
  }

  #the most common mode in each journey
  this.data$seg.common<-numeric(length(this.data[,1]))

  loop.list<-as.numeric(levels(factor(this.data$segment2)))

  for (j.seg in 1:length(loop.list)){
    this.j<-subset(this.data,segment2==loop.list[j.seg])
    this.data$seg.common[this.data$segment2==loop.list[j.seg]]<-names(sort(table(this.j$pred),decreasing=T)[1])
  }
  this.data$seg.common<-as.factor(this.data$seg.common)

  #return a variable, which is the most common mode in each journey
  return(this.data$seg.common)
}
