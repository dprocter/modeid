##########################
cross.validator<-function(training.dataset,formula,cv.marker){
  total.data<-training.dataset
  total.data$cv.marker<-cv.marker

  fit<-as.list(numeric(max(cv.marker)))
  pred<-as.list(numeric(max(cv.marker)))
  datasets<-as.list(numeric(max(cv.marker)))
  conf.mats<-as.list(numeric(max(cv.marker)))
  accs<-as.list(numeric(max(cv.marker)))


  for (i in 1:max(total.data$cv.marker)){
    reduced.train<-subset(total.data,cv.marker!=i)
    test<-subset(total.data,cv.marker==i)

    fit[[i]]<-randomForest::randomForest(formula,
                                         data=reduced.train,importance=TRUE
    )
    pred[[i]]<-predict(fit[[i]],test)
    test$pred.mode<-predict(fit[[i]],test)
    datasets[[i]]<-test
    conf.mats[[i]]<-confusion.matrix(datasets[[i]]$pred.mode,datasets[[i]]$true.mode)
  }
  return(cbind(fit,datasets,conf.mats,accs))
}
