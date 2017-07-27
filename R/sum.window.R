sum.window<-function(i, sum.var,time.var, window.length){
  prev.time<-time.var[i]-window.length/2
  next.time<-time.var[i]+window.length/2
  output<-sum(sum.var[time.var>=prev.time & time.var<=next.time])
  return(output)
}
