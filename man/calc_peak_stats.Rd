% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_peak_stats.R
\name{calc_peak_stats}
\alias{calc_peak_stats}
\title{Calculate summary statistics for each R peak}
\usage{
calc_peak_stats(
  times,
  signal,
  peaks,
  window = 10,
  qtmax = 60,
  rsmax = 20,
  omit_segments = NULL
)
}
\arguments{
\item{times}{Vector of times for ECG signals}

\item{signal}{Vector of ECG signal}

\item{peaks}{Location of R peaks as numeric indexes (from \code{\link[=detect_peaks]{detect_peaks()}} with \code{return_index=TRUE})}

\item{window}{Tight window around each peak to look for local max and min}

\item{qtmax}{Maximum width of QT interval to consider}

\item{rsmax}{Maximum width of RS interval to consider}

\item{omit_segments}{Segments that were omitted: a data frame with two
columns: the start and end index for each interval}
}
\value{
A data.frame with local max and min of ECG around peak,
height of T (local max before the next peak), RR intervals
to left and right of this R peak (in seconds), the ratio \code{leftRR}/\code{rightRR},
and the time from the R peak to the S trough (in milliseconds).
}
\description{
Calculate summary statistics for each R peak from ECG data, to help characterize which are normal and which are PVCs
}
\examples{
data(polar_h10)
bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks)
}
