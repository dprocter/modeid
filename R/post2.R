#### TODO
#1. make this take variable widths of window
#2. make this take variable percentage of window
#3. I don't think the dataset approach is necessary, remove it
#4. make it run with factors or numeric
#5. make this work for variable epochs
#6. examples



#' @title Post processing: local patterns
#' @description
#' Returns a variable, which identifies over 50% of neighbours within 4 minutes
#' they are of the same mode, and switches the central point to that mode
#' @param pred.variable
#' The variable you want processed, a factor
#' @param dataset
#' the dataset this is based on
#' @return
#' A variable, of the same format as \code{pred.variable}, with all points made the same
#' as those which have over 50% of points assigned to them within 4 minutes.
#' @details
#' We assume that if one mode is dominant both before and after the central point,
#' then that mode is likely what the central point should be. We therefore take a count of
#' each mode from two minutes before this point to two minutes after this point. If
#' a single mode has over half the points within the window, then the central point it
#' assigned to that mode.

post2<-function(pred.variable,dataset){

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
