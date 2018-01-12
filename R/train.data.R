#' @docType data
#'
#' @keywords datasets
#'
#' @title Training data from Procter et al (2018)
#'
#' @description The training dataset created in Procter et al (2018), minus identifiable information.
#'
#' @format A data frame containing 98387 rows and 35 variables
#'
#' \describe{
#' \item{id}{A marker distinguishing different participants}
#' \item{day}{Day of the week}
#' \item{date.time}{date and time variable}
#' \item{ax1.mad.4min}{median amplitude deviation of the horizontal axis of the accelerometer within 4 minutes}
#' \item{ax1.c90.4min}{90th centile of the horizontal axis of the accelerometer within 4 minutes}
#' \item{ax1.c10.4min}{10th centile of the horizontal axis of the accelerometer within 4 minutes}
#' \item{ax1.skew.4min}{skewness of the horizontal axis of the accelerometer within 4 minutes}
#' \item{ax1.kurt.4min}{kurtosis of the horizontal axis of the accelerometer within 4 minutes}
#' \item{ax2.mad.4min}{median amplitude deviation of the vertical axis of the accelerometer within 4 minutes}
#' \item{ax2.c90.4min}{90th centile of the vertical axis of the accelerometer within 4 minutes}
#' \item{ax2.c10.4min}{10th centile of the vertical axis of the accelerometer within 4 minutes}
#' \item{ax2.skew.4min}{skewness of the vertical axis of the accelerometer within 4 minutes}
#' \item{ax2.kurt.4min}{kurtosis of the vertical axis of the accelerometer within 4 minutes}
#' \item{ax3.mad.4min}{median amplitude deviation of the perpendicular axis of the accelerometer within 4 minutes}
#' \item{ax3.c90.4min}{90th centile of the perpendicular axis of the accelerometer within 4 minutes}
#' \item{ax3.c10.4min}{10th centile of the perpendicular axis of the accelerometer within 4 minutes}
#' \item{ax3.skew.4min}{skewness of the perpendicular axis of the accelerometer within 4 minutes}
#' \item{ax3.kurt.4min}{kurtosis of the perpendicular axis of the accelerometer within 4 minutes}
#' \item{ax1.fft.4min}{mean strength of the fast fourier transform signal for the horizontal
#' axis of the accelerometer within 4 minutes}
#' \item{ax2.fft.4min}{mean strength of the fast fourier transform signal for the vertical
#' axis of the accelerometer within 4 minutes}
#' \item{ax3.fft.4min}{mean strength of the fast fourier transform signal for the perpendicular
#' axis of the accelerometer within 4 minutes}
#' \item{spd.mean.4min}{mean speed within 4 minutes (kph)}
#' \item{spd.sd.4min}{standard deviation of speed within 4 minutes (kph)}
#' \item{spd.c10.4min}{10th centile of speed within 4 minutes (kph)}
#' \item{spd.c90.4min}{90th centile of speed within 4 minutes (kph)}
#' \item{sumsnr.4min}{mean sum of the signal to noise ratio within 4 minutes (kph)}
#' \item{spd.mean.4min}{mean speed within 4 minutes}
#' \item{near.train.4min}{mean distance to a train line within 4 minutes (m)}
#' \item{dist.next.4min}{mean distance moved over the next miute within 4 minutes (m)}
#' \item{dist.last.4min}{mean distance moved over the last minute within 4 minutes (m)}
#' \item{abs.acc.mean.4min}{mean absolute acceleration (from GPS) within 4 minutes}
#' \item{acc.sd.4min}{standard deviation of acceleration (fromGPS) within 4 minutes (kph)}
#' \item{lowsp.prop.4min}{mean proportion of points under 2kph within 4 minutes}
#' \item{true.mode}{true travel mode of the point (5 travel modes)}
#' \item{cv.marker}{marker used for cross-validation}
#' \item{true.mode.bus}{true travel mode of the point including buses (6 travel modes)}
#' }
#' 
#'@examples
#'data(train.data)
#'summary(train.data)
#'

"train.data"
