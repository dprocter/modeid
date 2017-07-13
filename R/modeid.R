#' @title  modeid: Identification of travel from objectively measured accelerometry and gps data
#' @section what it does
#' We include all the necessary functions to merge GPS and accelerometry data
#' (\code{\link{gps.acc.merger}}), clean gps data (\code{\link{gps.cleaner}})
#' , identify accelerometer non wear time (\code{\link{gps.acc.merger}}).
#'
#' We then include functions to fit and crossvalidate the machine learning algorithm
#' \code{\link[randomForest]{randomForest}}, measure cross-validated model accuracy
#'  and to smooth predictions once predicted. The machine learning algorithms supported
#'  will increase.
#'
#' @section generlisability
#' Currently we only manage Actigraph accelerometry data and Qstarz GPS data
#' (or other GPS data with a similar file structure).
#' This list will increase, please contact the author (duncan.procter@bristol.ac.uk)
#' if you need help with your data.
#'
