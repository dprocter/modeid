# post processing rule 1
post1<-function(pred.variable,dataset){
  this.data<-dataset
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
