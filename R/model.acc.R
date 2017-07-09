model.acc<-function(conf.mat){
  no.modes<-length(conf.mat[,1])

  precision<-numeric(no.modes)
  recall<-numeric(no.modes)
  modes<-rownames(conf.mat)

  for (i in 1:no.modes){
    precision[i]<-conf.mat[i,i]/sum(conf.mat[i,])*100
    recall[i]<-conf.mat[i,i]/sum(conf.mat[,i])*100
  }
  return(data.frame(modes,precision,recall))
}
