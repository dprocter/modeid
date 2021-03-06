#' @docType data
#'
#' @keywords datasets
#'
#' @title Training data from Procter et al (2017)
#'
#' @description The training dataset created in Procter et al (2017), minus identifiable information.
#'
#' @format A data frame containing 131573 rows and 36 variables
#'
#' \describe{
#' \item{Axis1}{Accelerometer axis 1 (vertical) counts}
#' \item{Axis2}{Accelerometer axis 2 (horizontal) counts}
#' \item{Axis3}{Accelerometer axis 3 (perpendicular) counts}
#' \item{ID}{A marker distinguishing different participants}
#' \item{day}{Day of the week}
#' \item{speed}{Speed in km/h}
#' \item{pdop}{Perpendicular dilution of precision}
#' \item{hdop}{Horizontol dilution of precision}
#' \item{vdop}{Vertical dilution of precision}
#' \item{sumsnr}{Sum of the signal to noise ratio, a measure of signal quality}
#' \item{near.train}{How close the point is to a train line (m)}
#' \item{dist.next.min}{distance to the point in 1 minutes time}
#' \item{dist.last.min}{distance to the point 1 minute before}
#' \item{ax1.mean}{Mean axis 1 counts within a 4 minute moving window}
#' \item{ax1.sd}{Axis 1 count SD within a 4 minute moving window}
#' \item{ax1.cent.10}{10th percentile of axis 1 counts within a 4 minute window}
#' \item{ax1.cent.90}{90th percentile of axis 1 counts within a 4 minute window}
#' \item{spd.mean}{Mean speed within a 4 minute moving window}
#' \item{spd.sd}{Speed SD within a 4 minute moving window}
#' \item{spd.cent.10}{10th percentile of speed within a 4 minute window}
#' \item{spd.cent.90}{90th percentile of speed within a 4 minute window}
#' \item{sumsnr.mean}{Mean sumSNR within a 4minute moving window}
#' \item{near.train}{Mean distance to train lines within a 4minute moving window}
#' \item{dist.4min.nextmin}{Mean distance moved in the next minute within a 4minute moving window}
#' \item{dist.4min.lastmin}{Mean distance moved over the last minute within a 4minute moving window}
#' \item{cv.marker}{The marker used for cross-validation, which separates the
#' participants into 5 randomly selected groups}
#' \item{true.mode}{The known travel mode of that point}
#' }
#'
#'@examples
#'data(Proc2017tdata)
#'summary(Proc2017.tdata)
#'

"Proc2017tdata"
