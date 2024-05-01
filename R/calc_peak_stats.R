#' Calculate summary statistics for each R peak
#'
#' Calculate summary statistics for each R peak from ECG data, to help characterize which are normal and which are PVCs
#'
#' @param peaks Location of R peaks as numeric indexes (from [detect_peaks()] with `return_index=TRUE`)
#'
#' @param signal Vector of ECG signal
#'
#' @param window Tight window around each peak to look for local max and min
#'
#' @param qtmax Maximum width of QT interval to consider
#'
#' @return A data.frame with local max and min of ECG around peak,
#'     height of T (local max before the next peak), RR intervals
#'     to left and right of this R peak, and the ratio `leftRR`/`rightRR`.
#'
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_peaks(h10$ecg)
#' peakstats <- calc_peak_stats(peaks, h10$ecg)

calc_peak_stats <-
    function(peaks, signal, window=10, qtmax=60)
{
    diff_peaks <- diff(peaks)

    result <- data.frame(
        pmax=vapply(peaks, function(a) max(signal[(a-window):(a+window)], na.rm=TRUE), 1.0),
        pmin=vapply(peaks, function(a) min(signal[(a-window):(a+window)], na.rm=TRUE), 1.0),
        Tmax=vapply(peaks, function(a) max(signal[(a+window):(a+qtmax)], na.rm=TRUE), 1.0),
        leftRR=c(NA, diff_peaks),
        rightRR=c(diff_peaks, NA)
    )

    result$RRratio <- result$leftRR/result$rightRR

    rownames(result) <- peaks
    result
}
