#' Find bad segments in ECG signal
#'
#' Find particularly noisy regions in ECG signal, with large values far from 0
#'
#' @param time Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param signal Numeric vector of ECG signal
#'
#' @param absval_thresh Threshold on absolute value of signal
#'
#' @param runmean_thresh Threshold on running mean of absolute value of signal
#'
#' @param window Window for running mean (in seconds)
#'
#' @param min_gap Minimum gap between bad segments (otherwise merged)
#'
#' @param pad Pad the bad segments by this many values on each end (in index values)
#'
#' @param tz Timezone used by [convert_timestamp()]
#'
#' @return Data frame with two columns: the start and end index for
#' each identified bad segment.
#'
#' @importFrom broman runningmean
#' @export
#'
#' @examples
#' data(h10)
#' empty_df <- find_bad_segments(h10$time, h10$ecg)
#'
#' h10$ecg[3896:3931] <- 2.5
#' find_bad_segments(h10$time, h10$ecg)

find_bad_segments <-
    function(time, signal, absval_thresh=2, runmean_thresh=0.7,
             window=0.2, min_gap=2000, pad=400, tz=Sys.timezone())
{
    stopifnot(length(time) == length(signal))

    time <- convert_timestamp(time, tz=tz)

    running_mean <- broman::runningmean(time, abs(signal), time, window=0.2)

    w <- which(abs(signal) > absval_thresh | running_mean > runmean_thresh)

    if(length(w)==0) {
        return(data.frame(start=numeric(0), end=numeric(0)))
    }

    dw <- diff(w)
    br <- which(dw > min_gap)

    badsegs <- matrix(ncol=2, nrow=length(br)+1)
    badsegs[1,1] <- w[1]
    for(i in seq_along(br)) { badsegs[i+1,1] <- w[br[i]+1]; badsegs[i,2] <- w[br[i]] }
    badsegs[nrow(badsegs),2] <- max(w)

    colnames(badsegs) <- c("start", "end")

    # pad the bad segments on each end
    badsegs[,1] <- badsegs[,1] - pad
    badsegs[,2] <- badsegs[,2] + pad
    badsegs[badsegs < 1] <- 1
    badsegs[badsegs > length(time)] <- length(time)

    as.data.frame(badsegs)
}
