
#' @export
#' @title Model accuracy
#' @description
#' Returns model accuracy from a confusion matrix derived from
#' \code{\link{confusion.matrix}}
#' @param conf.mat a confusion matrix generated by \code{\link{confusion.matrix}}
#' @return A seven column data.frame, with accuracy scores per mode
#' @details
#' Return accuracy scores for each mode, all expressed as percentages. 
#' There are many names for different accuracy scores, and some scores are preferential in certain
#'  situations, see https://en.wikipedia.org/wiki/Sensitivity_and_specificity
#'
#' Scores output:
#' 
#' 1. PPV, Positive predictive value (aka Precision): the percentage of points which the model predicts as each mode
#' which are also observed as that mode
#'
#' 2. Sensitivity (aka Recall, Detection rate, True Positive rate): the ratio of true positives
#' to true positives and false negatives
#'
#' 3. NPV: Negative predictive value: The ratio of true negatives and false negatives
#'
#' 4. Specificity (aka True Negative Rate): The ratio of true negatives to true negatives and false positives
#'
#' 5. Accuracy: The sum of true positives and negatives divided by the sum of true and false negatives and positives
#'
#' 6. F1 score: The harmonic mean of PPV and sensitivity
#'
#' @examples
#' pred<-numeric(10)+1
#' pred[5:6]<-2
#' pred<-factor(pred,labels=c("Mode1","Mode2"))
#'
#' obs<-numeric(10)+1
#' obs[6:8]<-2
#' obs<-factor(obs,labels=c("Mode1","Mode2"))
#'
#' model.acc(confusion.matrix(pred,obs))

model.acc<-function(conf.mat){
  no.modes<-length(conf.mat[,1])-1

  true.positives<-numeric(no.modes)
  true.negatives<-numeric(no.modes)
  false.positives<-numeric(no.modes)
  false.negatives<-numeric(no.modes)

  modes<-rownames(conf.mat[1:no.modes,])

  for (i in 1:no.modes){
    true.positives[i]<-conf.mat[i,i]
    true.negatives[i]<-sum(conf.mat[-i,-i])
    false.positives[i]<-sum(conf.mat[i,-i])
    false.negatives[i]<-sum(conf.mat[-i,i])
  }

  ppv<-true.positives/(true.positives+false.positives)*100
  sensitivity<-true.positives/(true.positives+false.negatives)*100
  npv<-true.negatives/(true.negatives+false.negatives)*100
  specificity<-true.negatives/(true.negatives+false.positives)*100
  accuracy<-(true.positives+true.negatives)/(true.positives+false.positives+false.negatives+true.negatives)*100
  f1.score<-(ppv*sensitivity/(ppv+sensitivity))*2

  return(data.frame(modes,ppv,sensitivity,npv,specificity,accuracy,f1.score))
}
