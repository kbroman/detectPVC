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
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' polar_h10$ecg_z <- zero_segments(polar_h10$ecg, bad_segs)
#' plot_ecg_mult(polar_h10$time, polar_h10$ecg_z, hilit_segments=bad_segs)

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
