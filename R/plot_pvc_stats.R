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
#' @importFrom broman time_axis
#' @importFrom graphics par axis
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_peaks(h10$ecg)
#' peak_stats <- calc_peak_stats(peaks, h10$ecg)
#' pvc <- (peak_stats$RSdist > 6)
#'
#' pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, n_at=4)
#' plot(pvc_stats)

plot.pvc_stats <-
    function(x,
             ylab_pvc="Percent PVC", ylab_hr="Heart rate (BPM)",
             ylim_pvc=c(0, max(x$pvc)*1.02), ylim_hr=c(range(x$hr))*c(0.98, 1.02),
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
        xax <- broman::time_axis(x$time)

        if(is.null(mgp.x)) mgp.x <- mgp
        if(is.null(mgp.y)) mgp.y <- mgp

        par(mfrow=c(2,1))
        grayplot(x$time, x$pvc, type=type, ..., xat=NA, ylab=ylab_pvc, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim_pvc, col=col, lwd=lwd)
        axis(side=1, mgp=mgp.x, at=xax$x, labels=xax$label, tick=FALSE)

        grayplot(x$time, x$hr, type=type, ..., xat=NA, ylab=ylab_hr, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim_hr, col=col, lwd=lwd)
        axis(side=1, mgp=mgp.x, at=xax$x, labels=xax$label, tick=FALSE)
    }

    internal_plot(x, ...)
    invisible()
}
