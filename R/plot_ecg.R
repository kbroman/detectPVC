#' Plot ECG signal
#'
#' Plot of ECG signal approximately matching traditional ECG graphs
#'
#' @param times Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param signal ECG signal. If missing or NULL, we take `times` as the signal and
#' then fill in with times assuming Polar H10 signal rate.
#'
#' @param vlines.col Color of vertical grid lines at seconds
#'
#' @param vlines.minor.col Color of vertical grid lines at 1/5 seconds
#'
#' @param bgcolor Background color
#'
#' @param tz Timezone (in converting times)
#'
#' @param ... Passed to [broman::grayplot()]
#'
#' @return None.
#'
#' @importFrom broman grayplot
#' @importFrom graphics axis
#' @importFrom lubridate hour minute second
#'
#' @export
#'
#' @examples
#' data(h10)
#' plot_ecg(h10$ecg)
#' plot_ecg(h10$time, h10$ecg)

plot_ecg <-
    function(times, signal, vlines.col="#c0a", vlines.minor.col="#f5c", bgcolor="gray98", tz=Sys.timezone(), ...)
{

    if(missing(signal) || is.null(signal)) {
        signal <- times
        times <- seq(from=0, length.out=length(signal), by=7.684608e-3) # signal rate for Polar H10
    } else {
        times <- convert_timestamp(times, tz=tz)
    }

    # internal function that gets called
    plot_ecg_internal <-
        function(times, signal,
                 vlines.col="#c0a", vlines.minor.col="#f5c",
                 vlines.lwd=1, vlines.minor.lwd=0.5,
                 vlines.lty=1, vlines.minor.lty=1,
                 hlines.col=NULL, hlines.minor.col=NULL,
                 hlines.lwd=NULL, hlines.minor.lwd=NULL,
                 hlines.lty=NULL, hlines.minor.lty=NULL,
                 ylim=c(-2, 2), xaxs="i", yaxs="i", type="l",
                 mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL,
                 bgcolor=bgcolor, las=1, lwd=2, xlab="Time (sec)", ylab="ECG signal",
                 ...)
    {
        ntimes <- as.numeric(times)

        major_xgrid <- seq(floor(min(ntimes)), ceiling(max(ntimes)), by=1)
        xgrid_times <- convert_timestamp(major_xgrid*1e9, tz)
        xgrid_lab <- lubridate::second(xgrid_times)

        # second axis with just minutes when it changes
        if(!is.numeric(times)) {
            xgrid_lab2 <- paste0(lubridate::hour(xgrid_times), ":", lubridate::minute(xgrid_times))
            xgrid_lab2[seq_along(xgrid_lab2) != 2 & xgrid_lab != "0"] <- ""
        }

        vlines <- seq(floor(min(ntimes)), ceiling(max(ntimes)), by=0.2)

        if(is.null(hlines.col)) hlines.col <- vlines.col
        if(is.null(hlines.minor.col)) hlines.minor.col <- vlines.minor.col
        if(is.null(hlines.lwd)) hlines.lwd <- vlines.lwd
        if(is.null(hlines.minor.lwd)) hlines.minor.lwd <- vlines.minor.lwd
        if(is.null(hlines.lty)) hlines.lty <- vlines.lty
        if(is.null(hlines.minor.lty)) hlines.minor.lty <- vlines.minor.lty

        vlines.col <- rep(c(vlines.col, vlines.minor.col), c(1,4))
        vlines.lwd <- rep(c(vlines.lwd, vlines.minor.lwd), c(1,4))
        vlines.lty <- rep(c(vlines.lty, vlines.minor.lty), c(1,4))

        hlines <- seq(floor(ylim[1]), ceiling(ylim[2]), by=0.2)
        hlines.col <- rep(c(hlines.col, hlines.minor.col), c(1,4))
        hlines.lwd <- rep(c(hlines.lwd, hlines.minor.lwd), c(1,4))
        hlines.lty <- rep(c(hlines.lty, hlines.minor.lty), c(1,4))


        if(is.null(mgp.x)) mgp.x <- mgp
        if(is.null(mgp.y)) mgp.y <- mgp

        broman::grayplot(times, signal, type=type, xat=NA, xlab=xlab, ylab=ylab,
                         vlines=vlines, hlines=hlines,
                         vlines.col=vlines.col, vlines.lwd=vlines.lwd, vlines.lty=vlines.lty,
                         hlines.col=hlines.col, hlines.lwd=hlines.lwd, hlines.lty=hlines.lty,
                         xaxs=xaxs, yaxs=yaxs, bgcolor=bgcolor, ylim=ylim, lwd=lwd, ...)
        axis(at=major_xgrid, labels=xgrid_lab, side=1, mgp=mgp.x, tick=FALSE, las=las)
        if(!is.numeric(times)) { # add times
            axis(at=major_xgrid, labels=xgrid_lab2, side=3, mgp=mgp.x, tick=FALSE, las=las)
        }
    }



    plot_ecg_internal(times, signal, vlines.col=vlines.col, vlines.minor.col=vlines.minor.col,
                      bgcolor=bgcolor, ...)

    invisible()
}
