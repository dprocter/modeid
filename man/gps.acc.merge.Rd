% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gps.acc.merge.R
\name{gps.acc.merge}
\alias{gps.acc.merge}
\title{Merging GPS and accelerometer files}
\usage{
gps.acc.merge(acc.data, gpsfile, participant.id, epoch.length,
  british.time = FALSE, UTC.offset = 0)
}
\arguments{
\item{acc.data}{The accelerometer data to merge, processed with \code{\link{process.acc}}}

\item{gpsfile}{The GPS file to merge, a .csv}

\item{participant.id}{A unique identifier for the participant}

\item{epoch.length}{epoch.length in seconds, currently, 5, 10 or 15 seconds only tested}

\item{british.time}{boolean, whether or not we the study is in Britain, so we need to check if the data was collected
within BST and adjust GPS UTC timings by 1hour}

\item{UTC.offset}{Hours offset from UTC, e.g. 3 hours ahead, UTC.offset=3, 3 hours behind, UTC=(-3)}
}
\description{
Returns a data.frame with Accelerometer and GPS data merged by timestamp
}
\details{
Currently only tested with Qstarz GPS device files. If you need other types contect the author
, they can be included with ease
}
