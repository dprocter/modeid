post2<-function(pred.variable,dataset){

  this.data<-dataset
  this.data$pred<-pred.variable
  this.data$post<-pred.variable

  this.data$walk.pres<-numeric(length(this.data$pred))
  this.data$walk.pres[this.data$pred=="walk"]<-1
  this.data$walk.in.2<-rollapply(this.data$walk.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$cycle.pres<-numeric(length(this.data$pred))
  this.data$cycle.pres[this.data$pred=="cycle"]<-1
  this.data$cycle.in.2<-rollapply(this.data$cycle.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$vehicle.pres<-numeric(length(this.data$pred))
  this.data$vehicle.pres[this.data$pred=="vehicle"]<-1
  this.data$vehicle.in.2<-rollapply(this.data$vehicle.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$train.pres<-numeric(length(this.data$pred))
  this.data$train.pres[this.data$pred=="train"]<-1
  this.data$train.in.2<-rollapply(this.data$train.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$bus.pres<-numeric(length(this.data$pred))
  this.data$bus.pres[this.data$pred=="bus"]<-1
  this.data$bus.in.2<-rollapply(this.data$bus.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$nothing.pres<-numeric(length(this.data$pred))
  this.data$nothing.pres[this.data$pred=="nothing"]<-1
  this.data$nothing.in.2<-rollapply(this.data$nothing.pres,width=13,align="center",FUN=sum,fill=NA)

  this.data$post[this.data$walk.in.2>=9]<-"walk"
  this.data$post[this.data$cycle.in.2>=9]<-"cycle"
  this.data$post[this.data$vehicle.in.2>=9]<-"vehicle"
  this.data$post[this.data$train.in.2>=9]<-"train"
  this.data$post[this.data$bus.in.2>=9]<-"bus"
  this.data$post[this.data$nothing.in.2>=9]<-"nothing"

  return(this.data$post)
}
