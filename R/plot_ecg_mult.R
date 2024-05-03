#' Plot ECG signal across multiple panels
#'
#' Plot a longer window of ECG signals across multiple panels arranged in one column.
#'
#' @param times Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param signal ECG signal. If missing or NULL, we take `times` as the signal and
#' then fill in with times assuming Polar H10 signal rate.
#'
#' @param start Start time for beginning of first panel, as integer as from Polar H10,
#' datetimes, or character strings like `"2024-05-03 13:27"` or `"2024-05-03 13:27:00"`
#'
#' @param length Length of time for each panel in seconds
#'
#' @param n_panel Number of panels
#'
#' @param peaks Optional vector of peak indices; if provided, dots will be plotted at each
#' peak location
#'
#' @param pvc Optional vector of boolean indicators of whether the peaks are PVC or not.
#' If provided, should be the same length as `peaks` and will be used to color the points
#' at the peaks.
#'
#' @param peak_colors Vector of two colors to use to color the dots that indicate non-PVC
#' and PVC peaks, respectively
#'
#' @param peak_pch Point type to plot at peaks.
#'
#' @param tz Timezone (in converting times)
#'
#' @param ... Passed [plot_ecg()]
#'
#' @return None.
#'
#' @importFrom graphics par points
#'
#' @export
#'
#' @examples
#' data(h10)
#' plot_ecg_mult(h10$time, h10$ecg, start="2024-04-29 22:32", length=20, n_panel=2)
#'
#' peaks <- detect_peaks(h10$ecg)
#' peak_stats <- calc_peak_stats(peaks, h10$ecg)
#' pvc <- (peak_stats$RSdist > 6)
#' plot_ecg_mult(h10$time, h10$ecg, start="2024-04-29 22:32", length=20, n_panel=2,
#'               peaks=peaks, pvc=pvc)


plot_ecg_mult <-
    function(times, signal, start=NULL, length=30, n_panel=4,
             peaks=NULL, pvc=NULL, peak_colors=c("slateblue", "violetred"),
             peak_pch=16, tz=Sys.timezone(), ...)
{
    par(mfrow=c(n_panel, 1))

    times <- convert_timestamp(times, tz=tz)
    start <- convert_timestamp(start, tz=tz)

    if(is.null(start)) start <- times[1]
    if(start < times[1]) start <- times[1]

    stopifnot(length(times) == length(signal))

    if(!is.null(peaks)) {
        if(any(peaks < 1 | peaks > length(times))) {
            stop("peaks should be numeric indexes from 1 to ", length(times))
        }
    }
    if(!is.null(pvc)) {
        if(is.null(peaks)) {
            stop("If pvc is provided, peaks should also be provided")
        }
        stopifnot(length(peaks) == length(pvc))
    }
    else {
        pvc <- rep(FALSE, length(peaks))
    }


    for(i in seq_len(n_panel)) {
        v <- get_time_interval(times, start, length=length)
        if(length(v)==0) next
        start <- start + length
        plot_ecg(times[v], signal[v], ...)

        if(!is.null(peaks)) {
            p <- peaks[peaks %in% v]
            ppvc <- pvc[peaks %in% v]
            points(times[p], signal[p], pch=peak_pch, col=peak_colors[ppvc+1])
        }

    }

}