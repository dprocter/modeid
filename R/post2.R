#### TODO
#1. make this for time windows, not continuous windows
#4. make it run with factors or numeric
#6. examples


#' @export
#' @title Post processing: local patterns
#' @description
#' Returns a variable, which identifies over 50% of neighbours within 4 minutes
#' they are of the same mode, and switches the central point to that mode
#' @param pred.variable
#' The variable you want processed, a factor
#' @param epoch.length
#' The length of the epoch you are using, in seconds
#' @param window.width
#' How wide a window you want to smooth, in seconds
#' @param prop.agreement
#' What proportion of that window you want to have the same mode
#' @return
#' A variable, of the same format as \code{pred.variable}, with all points made the same
#' as those which have over the specified proportion of the specified window.
#' @details
#' We assume that if one mode is dominant both before and after the central point,
#' then that mode is likely what the central point should be. We therefore take a count of
#' each mode from two minutes before this point to two minutes after this point. If
#' a single mode has over half the points within the window, then the central point it
#' assigned to that mode.

post2<-function(pred.variable, epoch.length, window.width, prop.agreement){
  pred<-pred.variable
  post<-pred.variable

  t.modes<-levels(pred.variable)

  for (i in 1:length(t.modes)){
    this.mode.present<-numeric(length(pred))
    this.mode.present[pred==t.modes[i]]<-1
    this.mode.counter<-zoo::rollapply(this.mode.present,width= (window.width/epoch.length)+1 ,align="center",FUN=sum,fill=NA)

    agreement.epochs<-ceiling(prop.agreement*(window.width/epoch.length))
    post[this.mode.counter>=agreement.epochs]<-t.modes[i]
  }

  return(post)
}

