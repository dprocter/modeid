full.cv<-function(cvd.model,full.dataset){
  conf.mats<-as.list(numeric(5))
  
  for (i in 1:5){
    data.with.nas<-full.dataset
    predictable.dataset<-subset(full.dataset,select=-c(cv.marker,true.mode))
    data.with.nas$pred<-predict(cvd.model[,1][[i]],newdata=as.matrix(predictable.dataset))
    data.with.nas$pred<-factor(data.with.nas$pred,labels=levels(data.with.nas$true.mode))
    
    this.fold.data<-subset(data.with.nas,cv.marker==i)
    
    conf.mats[[i]]<-confusion.matrix(this.fold.data$pred,this.fold.data$true.mode)
  }
  
  overall.cv<-conf.mats[[1]]+conf.mats[[2]]+conf.mats[[3]]+conf.mats[[4]]+conf.mats[[5]]
  overall.acc<-model.acc(conf.mats[[1]]+conf.mats[[2]]+conf.mats[[3]]+conf.mats[[4]]+conf.mats[[5]])
  return(list(overall.cv,overall.acc))
  
}
