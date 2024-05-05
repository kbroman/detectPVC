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
#' @param rsmax Maximum width of RS interval to consider
#'
#' @param omit_segments Segments that were omitted: a data frame with two
#' columns: the start and end index for each interval
#'
#' @return A data.frame with local max and min of ECG around peak,
#'     height of T (local max before the next peak), RR intervals
#'     to left and right of this R peak, the ratio `leftRR`/`rightRR`,
#'     and the distance from the R peak to the S trough.
#'
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_peaks(h10$ecg)
#' peakstats <- calc_peak_stats(peaks, h10$ecg)

calc_peak_stats <-
    function(peaks, signal, window=10, qtmax=60, rsmax=20,
             omit_segments=NULL)
{
    diff_peaks <- diff(peaks)
    max_index <- length(signal)

    result <- data.frame(
        pmax=vapply(peaks, function(a) {
            v <- (a-window):(a+window)
            v <- v[v >= 1 & v <= max_index]
            max(signal[v], na.rm=TRUE)}, 1.0),
        pmin=vapply(peaks, function(a) {
            v <- (a-window):(a+window)
            v <- v[v >= 1 & v <= max_index]
            min(signal[v], na.rm=TRUE)}, 1.0),
        Tmax=vapply(peaks, function(a) {
            v <- (a+window):(a+qtmax)
            v <- v[v >= 1 & v <= max_index]
            if(length(v)==0) return(NA)
            max(signal[v], na.rm=TRUE)}, 1.0),
        leftRR=c(NA, diff_peaks),
        rightRR=c(diff_peaks, NA),
        RRratio=rep(NA, length(peaks)),
        RSdist=vapply(peaks, function(a) {
            v <- a:(a+rsmax)
            v <- v[v >= 1 & v <= max_index]
            which.min(signal[v])}, 1)
    )

    # if omitted segments, need to censor leftRR and rightRR
    if(!is.null(omit_segments)) {
        peaks_real <- c(rep(TRUE, length(peaks)), rep(FALSE, nrow(omit_segments)))
        peaks_w_omit <- c(peaks, rowMeans(omit_segments[,1:2,drop=FALSE]))
        o <- order(peaks_w_omit)
        peaks_w_omit <- peaks_w_omit[o]
        peaks_real <- peaks_real[o]

        n <- length(peaks_real)
        omit2left <- peaks_w_omit[-1][peaks_real[-1] & !peaks_real[-n]]
        omit2right <- peaks_w_omit[-n][peaks_real[-n] & !peaks_real[-1]]

        if(length(omit2left) > 0) {
            result[peaks %in% omit2left, "leftRR"] <- NA
        }
        if(length(omit2right) > 0) {
            result[peaks %in% omit2right, "rightRR"] <- NA
        }
    }


    result$RRratio <- result$leftRR/result$rightRR

    rownames(result) <- peaks
    result
}
