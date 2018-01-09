#' @export
#' @title Post processing,: neighbour matching
#' @description Returns a variable, which has identified predicted points where
#' one point is neighboured by two of a different mode. The central
#' point is then set as the mode of it's neighbours
#' @param pred.variable The variable you wish to smooth
#' @return
#' A variable of travel mode, single mis-classifications removed
#' @details
#' If both of a points neighbours differ to the center point and
#' are the same as one another, we assume that this is a prediction
#' error. This function makes the center point the same as it's neighbours,
#' providing they are the same as one another
#' #' @examples
#' eg.sequence<-rep(seq(1,3,1),each=6)
#' eg.sequence[8:10]<-1
#' eg.sequence<-factor(eg.sequence,labels=c("walk","run","cycle"))
#' eg.sequence
#' post1(pred.variable = eg.sequence, dataset = data.frame(eg.sequence))

# post processing rule 1
post1<-function(pred.variable){
  this.data<-data.frame(pred.variable)
  this.data$pred<-pred.variable
  #converts the factor to a numeric, to avoid the problem of NAs in
  this.data$pred.num<-as.numeric(this.data$pred)
  #set NAs as 0, this will be changed back before export
  this.data$pred.num[is.na(this.data$pred.num)]<-0
  this.data$post<-this.data$pred.num

  #a variable that shift the predictions by 1 forward
  this.data$prev.pred<-this.data$pred.num
  this.data$prev.pred[1]<-0
  this.data$prev.pred[2:length(this.data$prev.pred)]<-this.data$pred.num[1:length(this.data$prev.pred)-1]
  #a variable that shifts the predictions by 1 backward
  this.data$next.pred<-this.data$pred.num
  this.data$next.pred[length(this.data$next.pred)]<-0
  this.data$next.pred[1:length(this.data$next.pred)-1]<-this.data$pred.num[2:length(this.data$next.pred)]

  # take only those whose points neighbours are of the same mode and different to the center
  # set them to be the same mode as their neighbours
  this.data$post[this.data$prev.pred==this.data$next.pred & this.data$prev.pred!=this.data$pred.num
                 & this.data$prev.pred!=0]<-this.data$prev.pred[
                   this.data$prev.pred==this.data$next.pred & this.data$prev.pred!=this.data$pred.num
                   & this.data$prev.pred!=0]

  # set the 0's back to NAS
  this.data$post[this.data$post==0]<-NA
  # convert the numeric back into a factor
  this.data$post<-factor(this.data$post)
  #assigned the original factor levels to the factor
  # a somewhat convoluted method, which allows for the fact that you might have reduced the
  # original number of factor levels
  levels(this.data$post)<-levels(this.data$pred)[as.numeric(levels(this.data$post))]

  return(this.data$post)

}
