### confusion matrix
confusion.matrix<-function(predicted,observed){
  t.modes<-levels(observed)
  conf.creator<-data.frame(predicted,observed)
  conf.mat<-matrix(nrow=length(t.modes),ncol=length(t.modes))
  rownames(conf.mat)<-t.modes
  colnames(conf.mat)<-t.modes
  for (i in 1:length(t.modes)){
    for (j in 1:length(t.modes)){
      conf.mat[i,j]<-length(conf.creator$predicted[conf.creator$predicted==t.modes[i] &
                                                     conf.creator$observed==t.modes[j]])
    }
  }
  return(conf.mat)
}
