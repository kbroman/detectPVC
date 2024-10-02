#' Adjust R peaks to hit local max
#'
#' Adjust the estimated R peaks from `rsleep::detect_rpeaks()` to hit the local maximum.
#'
#' @param peaks Estimated R peaks (as index)
#'
#' @param signal ECG signal vector
#'
#' @param window Window (+/-) to look for the local peak
#'
#' @return Vector of peaks, adjusted to hit the local max
#'
#' @seealso [detect_peaks()]
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$ecg, adjust=FALSE, omit_segments=bad_segs)
#' peaks_adj <- adjust_peaks(peaks, polar_h10$ecg)

adjust_peaks <-
    function(peaks, signal, window=10)
{

    vapply(peaks, function(a) { v <- (a-window):(a+window); v[which.max(signal[v])] }, 1)

}
