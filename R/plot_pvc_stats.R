#' Plot running PVC statistics
#'
#' Plot running PVC statistics from [running_pvc_stats()]
#'
#' @param pvc_stats Data frame of running PVC stats from [running_pvc_stats()]
#'
#' @param ... Passed to [broman::grayplot()]
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
    function(x, ...)
{
    old_mfrow <- par("mfrow")
    old_mar <- par("mar")
    on.exit(par(mfrow=old_mfrow, mar=old_mar))

    internal_plot <-
        function(x, ..., xlab="Time", ylab1="Percent PVC",
                 ylab2="Heart rate (BPM)", type="l",
                 mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL,
                 mar=c(3.1, 3.6, 1.6, 0.6), xaxs="i", yaxs="i",
                 ylim1=c(0, max(x$pvc)*1.02),
                 ylim2=c(range(x$hr))*c(0.98, 1.02),
                 col="darkslateblue", lwd=2)
    {
        par(mar=mar)
        xax <- broman::time_axis(x$time)

        if(is.null(mgp.x)) mgp.x <- mgp
        if(is.null(mgp.y)) mgp.y <- mgp

        par(mfrow=c(2,1))
        grayplot(x$time, x$pvc, type=type, ..., xat=NA, ylab=ylab1, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim1, col=col, lwd=lwd)
        axis(side=1, mgp=mgp.x, at=xax$x, label=xax$label, tick=FALSE)

        grayplot(x$time, x$hr, type=type, ..., xat=NA, ylab=ylab2, xlab=xlab,
                 xaxs=xaxs, yaxs=yaxs, ylim=ylim2, col=col, lwd=lwd)
        axis(side=1, mgp=mgp.x, at=xax$x, label=xax$label, tick=FALSE)
    }

    internal_plot(x, ...)
    invisible()
}
