#' Plot running PVC statistics
#'
#' Plot running PVC statistics from [running_pvc_stats()]
#'
#' @param x Data frame of running PVC stats from [running_pvc_stats()]
#'
#' @param ylab_pvc Y-axis label for the plot of percent PVC
#'
#' @param ylab_hr  Y-axis label for the plot of heart rate
#'
#' @param ylim_pvc Y-axis limits for the plot of percent PVC
#'
#' @param ylim_hr Y-axis limits for the plot of heart rate
#'
#' @param ... Passed to [broman::grayplot()]
#'
#' @details Makes a plot with two panels, of percent PVC vs time and heart rate vs time.
#'
#' @return None.
#'
#' @importFrom broman timeplot
#' @importFrom graphics par axis
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$time, polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#'
#' pvc_stats <- running_pvc_stats(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                                window=60, n_at=100)
#' plot(pvc_stats)

plot.pvc_stats <-
    function(x,
             ylab_pvc="Percent PVC", ylab_hr="Heart rate (BPM)",
             ylim_pvc=c(0, max(x$pvc, na.rm=TRUE)*1.02),
             ylim_hr=c(range(x$hr, na.rm=TRUE))*c(0.98, 1.02),
             ...)
{
    old_mfrow <- par("mfrow")
    old_mar <- par("mar")
    on.exit(par(mfrow=old_mfrow, mar=old_mar))

    internal_plot <-
        function(x, ..., xlab="Time", type="l",
                 mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL,
                 mar=c(3.1, 3.6, 1.6, 0.6), xaxs="i", yaxs="i",
                 col="darkslateblue", lwd=2)
    {
        par(mar=mar)
        if(is.null(mgp.x)) mgp.x <- mgp
        if(is.null(mgp.y)) mgp.y <- mgp

        par(mfrow=c(2,1))
        timeplot(x$time, x$pvc, type=type, ..., ylab=ylab_pvc, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim_pvc, col=col, lwd=lwd)

        timeplot(x$time, x$hr, type=type, ..., ylab=ylab_hr, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim_hr, col=col, lwd=lwd)
    }

    internal_plot(x, ...)
    invisible()
}
