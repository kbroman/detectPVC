% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_pvc_pattern.R
\name{find_pvc_pattern}
\alias{find_pvc_pattern}
\title{Find PVC patterns}
\usage{
find_pvc_pattern(
  times,
  peaks,
  pvc,
  omit_segments = NULL,
  pattern,
  min_length = 0,
  return_index = TRUE,
  tz = Sys.timezone()
)
}
\arguments{
\item{times}{Vector of times (integers as from Polar H10, datetimes, or character strings)}

\item{peaks}{Vector of peak indices as integers from 1 to \code{length(times)}}

\item{pvc}{Vector of boolean indicators of whether the peaks are PVC or not.
Should be the same length as \code{peaks}.}

\item{omit_segments}{Segments to be ignored in analysis, as a data frame with
two columns: the start and end index for each interval to be ignored}

\item{pattern}{The character string with the regular expression to look for,
with \code{N}=normal, \code{P}=PVC, so for example \code{"(NNP)+"} for trigeminy.}

\item{min_length}{Minimum length (as number of beats) for a pattern instance.}

\item{return_index}{If TRUE, return indexes to times, not the actual times.}

\item{tz}{Time zone for converting time stamps}
}
\value{
Data frame with start and end times + length of match (as number of peaks)
}
\description{
In a sequence of ECG peaks classified as PVC or not, find patterns
like bigeminy, trigeminy, couplets and triplets.
}
\examples{
data(h10)
peaks <- detect_peaks(h10$ecg)
peak_stats <- calc_peak_stats(peaks, h10$ecg)
pvc <- (peak_stats$RSdist > 6)

trigeminy <- find_pvc_pattern(h10$time, peaks, pvc, pattern="(NNP)+")
bigeminy <- find_pvc_pattern(h10$time, peaks, pvc, pattern="(NP)+", min_length=4)
couplets <- find_pvc_pattern(h10$time, peaks, pvc, pattern="PP+")
triplets <- find_pvc_pattern(h10$time, peaks, pvc, pattern="PP+", min_length=3)
}