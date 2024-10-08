% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_states.R
\name{summary_states}
\alias{summary_states}
\alias{summary.states}
\title{Summarize output of plot_states}
\usage{
summary_states(object)

\method{summary}{states}(object, ...)
}
\arguments{
\item{object}{An object of class \code{"states"}, as output by \code{\link[=plot_states]{plot_states()}}}

\item{...}{Ignored}
}
\value{
A data frame with the different possible PVC states
(normal, bigeminy, trigeminy, other, omitted, and total time)
and amount of time (in min) spent in each, as well as the percent of the
overall time.
}
\description{
Summarize output of plot_states, with amount of time in different PVC states
}
\examples{
data(polar_h10)
bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
pvc <- (peak_stats$RStime > 50)

st <- plot_states(polar_h10$time, peaks, pvc, omit_segments=bad_segs, xlim=c(50, 60), draw=FALSE)
summary(st)
}
\seealso{
\code{\link[=plot_states]{plot_states()}}
}
