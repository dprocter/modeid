#' @export
#'
#'
post2.time<-function(pred.variable, epoch.length, window.width, prop.agreement, dataset){
  pred<-pred.variable
  post<-pred.variable
  this.data<-dataset
  #this.data$date.time<-strptime(paste(test.dataset$date.txt,test.dataset$time.txt,sep=" "),format="%Y/%m/%d %H:%M:%S")
  this.data$date.time<-strptime(paste(this.data$datetxt, this.data$timetxt, sep=" "),format="%d/%m/%Y %H:%M:%S")
  this.data$time.sec<-unclass(as.POSIXct(this.data$date.time))

  if (is.numeric(pred)){
    t.modes<-as.numeric(levels(factor(pred)))
  } else{
    t.modes<-levels(pred)
  }

  for (i in 1:length(t.modes)){
    this.mode.present<-numeric(length(pred))
    this.mode.present[pred==t.modes[i]]<-1

    this.mode.counter<-foreach::foreach(j=seq(1,length(this.data[,1]),1),.combine='c', .packages = "modeid") %dopar% sum.window(
      j, sum.var= this.mode.present, time.var = this.data$time.sec, window.length = 120)

    #the window width is 1 wider than the designated window width, in epochs
    #this.mode.counter<-zoo::rollapply(this.mode.present,width= (window.width/epoch.length)+1 ,align="center",FUN=sum,fill=NA)
    #agreement.epochs<-ceiling(0.5*(120/10))
    agreement.epochs<-ceiling(prop.agreement*(window.width/epoch.length))
    post[this.mode.counter>=agreement.epochs]<-t.modes[i]
  }

  return(post)
}
