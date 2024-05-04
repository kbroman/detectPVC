#' Zero out segments of a ECG signal
#'
#' Replace ECG signal in a set of intervals with 0
#'
#' @param signal Numeric vector of ECG signal
#'
#' @param segments Data frame with two columns: the start and end index for
#' each interval that is to have signal replaced by 0
#'
#' @return The input `signal` vector, but with the specified intervals replaced with 0.
#'
#' @export
#'
#' @examples
#' data(h10)
#' h10$ecg[3896:3931] <- 2.5  # stick in some bad data
#' badseg <- find_bad_segments(h10$time, h10$ecg)
#' h10$ecg_z <- zero_segments(h10$ecg, badseg)

zero_segments <-
    function(signal, segments)
{
    stopifnot(ncol(segments)==2)
    stopifnot(all(segments[,2] >= segments[,1]))
    stopifnot(all(segments >= 1 & segments <= length(signal)))

    for(i in seq_len(nrow(segments))) {
        signal[segments[i,1] : segments[i,2]] <- 0
    }

    signal
}
