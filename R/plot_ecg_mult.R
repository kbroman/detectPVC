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
#' @param n_panel Number of panels.
#'
#' @param peaks Optional vector of peak indices; if provided, dots will be plotted at each
#' peak location
#'
#' @param pvc Optional vector of boolean indicators of whether the peaks are PVC or not.
#' If provided, should be the same length as `peaks` and will be used to color the points
#' at the peaks.
#'
#' @param hilit_segments Optional matrix indicating segments to highlight, as returned from
#' [find_bad_segments()], with rows corresponding to intervals and two columns of
#' numeric indexes of the start and end of each interval
#'
#' @param col_peak Vector of two colors to use to color the dots that indicate non-PVC
#' and PVC peaks, respectively
#'
#' @param pch_peak Vector of one or two point types to plot at peaks.
#'
#' @param cex_peak Vector of one or two values to indicate size of points
#' that indicate non-PVC and PVC peaks, respectively
#'
#' @param bg_peak Vector of one or two colors to use for background color for
#'     the dots that indicate non-PVC and PVC peaks, respectively.
#'
#' @param col_segments Color to highlight the segments in `hilit_segments`
#'
#' @param tz Timezone (in converting times)
#'
#' @param ... Passed [plot_ecg()]
#'
#' @return None.
#'
#' @details We use `par(mfrow=c(n_panel, 1))` before creating the set of panels.
#' However, in the case `n_panel==1`, we don't run `par(mfrow)` and so this can be
#' used in a more general way.
#'
#' @importFrom graphics par points lines rect
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' plot_ecg_mult(polar_h10$time, polar_h10$ecg, start="2024-05-05 09:52", length=20, n_panel=2)
#'
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$time, polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#' plot_ecg_mult(polar_h10$time, polar_h10$ecg, start="2024-05-05 09:50:30", length=20, n_panel=2,
#'               peaks=peaks, pvc=pvc, hilit_segments=bad_segs)

plot_ecg_mult <-
    function(times, signal, start=NULL, length=30, n_panel=4,
             peaks=NULL, pvc=NULL, hilit_segments=NULL,
             col_peak=c("slateblue", "violetred"),
             pch_peak=16, cex_peak=1, bg_peak=c("slateblue", "violetred"),
             col_segments="#cc00dd33", tz=Sys.timezone(), ...)
{
    old_mfrow <- par("mfrow")
    on.exit(par(mfrow=old_mfrow))

    if(n_panel > 1) par(mfrow=c(n_panel, 1))

    if(is.null(start)) start <- times[1]
    times <- convert_timestamp(times, tz=tz)
    start <- convert_timestamp(start, tz=tz)
    if(start < times[1]) start <- times[1]

    if(length(col_peak)==1) col_peak <- rep(col_peak, 2)
    if(length(pch_peak)==1) pch_peak <- rep(pch_peak, 2)
    if(length(cex_peak)==1) cex_peak <- rep(cex_peak, 2)
    if(length(bg_peak)==1)  bg_peak <- rep(bg_peak, 2)

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

    # if hilit_segments has times, convert to indexes
    if(!is.null(hilit_segments) && nrow(hilit_segments) > 0) {
        if("POSIXct" %in% class(hilit_segments[,1]) ||
           "POSIXt" %in% class(hilit_segments[,1])) {
            hilit_segments <- t(vapply(seq_len(nrow(hilit_segments)), function(i)
                range(get_time_interval(times, start=hilit_segments$start[i],
                                        end=hilit_segments$end[i], tz=tz)), c(1,2)))
        }
    }

    for(i in seq_len(n_panel)) {
        v <- get_time_interval(times, start, length=length)
        if(length(v)==0) next
        start <- start + length
        plot_ecg(times[v], signal[v], ...)

        if(!is.null(hilit_segments) && nrow(hilit_segments)>0) {

            hilit <- segments_contain_values(v, hilit_segments)
            if(any(hilit)) {
                u <- par("usr")
                for(i in which(hilit)) {
                    rect(times[hilit_segments[i,1]], u[3],
                         times[hilit_segments[i,2]], u[4],
                         col=col_segments, border=col_segments)
                }
                lines(times[v], signal[v])
            }
        }

        if(!is.null(peaks)) {
            p <- peaks[peaks %in% v]
            ppvc <- pvc[peaks %in% v]
            points(times[p], signal[p], pch=pch_peak[ppvc+1], col=col_peak[ppvc+1],
                   cex=cex_peak[ppvc+1], bg=bg_peak[ppvc+1])
        }


    }

}
