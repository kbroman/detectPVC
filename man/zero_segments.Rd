% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zero_segments.R
\name{zero_segments}
\alias{zero_segments}
\title{Zero out segments of a ECG signal}
\usage{
zero_segments(signal, segments)
}
\arguments{
\item{signal}{Numeric vector of ECG signal}

\item{segments}{Data frame with two columns: the start and end index for
each interval that is to have signal replaced by 0}
}
\value{
The input \code{signal} vector, but with the specified intervals replaced with 0.
}
\description{
Replace ECG signal in a set of intervals with 0
}
\examples{
data(polar_h10)
bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
polar_h10$ecg_z <- zero_segments(polar_h10$ecg, bad_segs)
plot_ecg_mult(polar_h10$time, polar_h10$ecg_z, hilit_segments=bad_segs)
}
