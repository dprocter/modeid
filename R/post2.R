post2.test<-function(pred.variable,dataset){

  this.data<-dataset
  this.data$pred<-pred.variable
  this.data$post<-pred.variable

  t.modes<-levels(pred.variable)

  for (i in 1:length(t.modes)){
    this.mode.present<-numeric(length(this.data[,1]))
    this.mode.present[this.data$pred==t.modes[i]]<-1
    this.mode.counter<-zoo::rollapply(this.mode.present,width=25,align="center",FUN=sum,fill=NA)

    this.data$post[this.mode.counter>=12]<-t.modes[i]
  }

  return(this.data$post)
}
