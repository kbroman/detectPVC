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
#' @param missing_thresh Maximum amount of missing data within 1 second (as a proportion)
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
#' @param sRate Signal rate, as expected number of data points per
#'     second; needed if `return_prop=TRUE`.
#'
#' @return Data frame with two columns: the start and end index for
#' each identified bad segment.
#'
#' @importFrom broman runningmean
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)

find_bad_segments <-
    function(time, signal, absval_thresh=2, runmean_thresh=0.7,
             missing_thresh=0.1, window=0.2, min_gap=2000, pad=400,
             return_index=TRUE, tz=Sys.timezone(), sRate=1e9/7682304)
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
    prop_data <- running_datacount(time, return_prop=TRUE, sRate=sRate)
    w <- which(prop_data < missing_thresh)
    if(length(w)>0) badsegs <- rbind(badsegs, vector_to_segments(w, min_gap))
    if(nrow(badsegs)==0) return(badsegs)

    if(nrow(badsegs) > 1) badsegs <- badsegs[order(badsegs[,1], badsegs[,2]),]

    # pad the bad segments on each end
    badsegs[,1] <- badsegs[,1] - pad
    badsegs[,2] <- badsegs[,2] + pad
    badsegs[badsegs < 1] <- 1
    badsegs[badsegs > length(time)] <- length(time)

    badsegs <- merge_overlaps(badsegs, min_gap-pad)

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
    function(badsegs, min_gap)
{
    # merge overlap
    d <- badsegs[-nrow(badsegs),2] - badsegs[-1,1]
    while(nrow(badsegs) > 1 && any(d > 0)) {
        omit <- NULL
        for(i in which(d > -min_gap)) {
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
    if(any(segs[,1] < 1 | segs[,2] < 1)) {
        stop("segs can't be negative")
    }
    n <- length(times)
    if(any(segs[,1] > n | segs[,2] > n)) {
        stop("segs values must be <= length(times)")
    }

    for(i in seq_along(segs)) {
        segs[,i] <- times[segs[,i]]
    }
    segs
}


segs_time_to_index <-
    function(segs, times)
{
    some_not_found <- FALSE
    for(i in seq_along(segs)) {
        segs[,i] <- match(segs[,i], times)

        if(any(is.na(segs[,i]))) {
            some_not_found <- TRUE
        }
    }

    if(some_not_found) {
        # (avoiding duplicate warnings)
        warning("some segs not found in times")
    }

    segs
}
