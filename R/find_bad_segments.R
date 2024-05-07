#' Find bad segments in ECG signal
#'
#' Find particularly noisy regions in ECG signal, with large values
#' far from 0, plus also regions with excessive missing data.
#'
#' @param time Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param signal Numeric vector of ECG signal
#'
#' @param absval_thresh Threshold on absolute value of signal
#'
#' @param runmean_thresh Threshold on running mean of absolute value of signal
#'
#' @param missing_thresh Maximum amount of missing data within 1 second
#'
#' @param window Window for running mean (in seconds)
#'
#' @param min_gap Minimum gap between bad segments (otherwise merged)
#'
#' @param pad Pad the bad segments by this many values on each end (in index values)
#'
#' @param return_index If TRUE, return indexes to `time`; otherwise return actual date/times.
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
             missing_thresh=70, window=0.2, min_gap=2000, pad=400,
             return_index=TRUE, tz=Sys.timezone())
{
    stopifnot(length(time) == length(signal))

    time <- convert_timestamp(time, tz=tz)

    # smooth absolute signal
    running_mean <- broman::runningmean(time, abs(signal), time, window=0.2)

    # regions with large values
    w <- which(abs(signal) > absval_thresh | running_mean > runmean_thresh)

    if(length(w)==0) {
        badsegs <- data.frame(start=numeric(0), end=numeric(0))
    }
    else badsegs <- vector_to_segments(w, min_gap)

    # look for regions with lots of missing data
    # count data points every sec
    n_data <- broman::runningmean(time, rep(1, length(time)), at=time, window=1, "sum")
    w <- which(n_data < 130-missing_thresh)
    if(length(w)>0) badsegs <- rbind(badsegs, vector_to_segments(w, min_gap))
    if(nrow(badsegs)==0) return(badsegs)

    if(nrow(badsegs) > 1) badsegs <- badsegs[order(badsegs[,1], badsegs[,2]),]

    # pad the bad segments on each end
    badsegs[,1] <- badsegs[,1] - pad
    badsegs[,2] <- badsegs[,2] + pad
    badsegs[badsegs < 1] <- 1
    badsegs[badsegs > length(time)] <- length(time)

    badsegs <- as.data.frame(badsegs)

    if(!return_index) badsegs <- segs_index_to_time(badsegs, time)

    badsegs
}


vector_to_segments <-
    function(w, min_gap=2000)
{
    dw <- diff(w)
    br <- which(dw > min_gap)

    badsegs <- matrix(ncol=2, nrow=length(br)+1)
    badsegs[1,1] <- w[1]
    for(i in seq_along(br)) { badsegs[i+1,1] <- w[br[i]+1]; badsegs[i,2] <- w[br[i]] }
    badsegs[nrow(badsegs),2] <- max(w)

    colnames(badsegs) <- c("start", "end")
    badsegs
}

merge_overlaps <-
    function(badsegs)
{
    # merge overlap
    d <- badsegs[-nrow(badsegs),2] - badsegs[-1,1]
    while(nrow(badsegs) > 1 && any(d > 0)) {
        omit <- NULL
        for(i in which(d > 0)) {
            badsegs[i+1,1] <- badsegs[i,1]
            omit <- c(omit, i)
        }
        badsegs <- badsegs[-omit,,drop=FALSE]
        d <- badsegs[-nrow(badsegs),2] - badsegs[-1,1]
    }

    badsegs
}


segs_index_to_time <-
    function(segs, times)
{
    for(i in seq_along(segs)) {
        segs[,i] <- times[segs[,i]]
    }
    segs
}


segs_time_to_index <-
    function(segs, times)
{
    for(i in seq_along(segs)) {
        segs[,i] <- match(segs[,i], times)
    }
    segs
}
