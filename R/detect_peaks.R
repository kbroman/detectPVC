#' Detect R peaks in a raw ECG signal, using a sliding window
#'
#' Detect R peaks in a raw ECG signal, using a modified version of
#' `rsleep::detect_rpeaks()` but using a sliding window.
#'
#' @param time Vector of times at which ECG signal was measured (integers as from Polar H10, datetimes, or character strings)
#'
#' @param signal Numerical vector of ECG signal
#'
#' @param window Integer indicating the number of values to consider at one time
#'
#' @param pad Integer indicating the number of values to consider on each side, surrounding the window
#'
#' @param sRate ECG signal rate
#'
#' @param peak_limit Limit factor on size of peaks relative to mean convolved signal.
#' Larger values will ignore low-valued peaks.
#'
#' @param max_dist Maximum distance allowed between adjacent peaks

#' @param omit_segments Segments to be omitted: a data frame with two
#' columns: the start and end index for each interval
#'
#' @param ... Passed to a modified version of `rsleep::detect_rpeaks()`
#'
#' @param return_index If TRUE, the index for each R peak is returned instead of the timing
#'
#' @param adjust If TRUE, adjust the results to hit the nearby local maxima
#'
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @importFrom stats convolve
#' @importFrom signal butter filtfilt
#' @importFrom parallel detectCores stopCluster makeCluster parLapply mclapply
#'
#' @seealso [adjust_peaks()]
#'
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$time, polar_h10$ecg, omit_segments=bad_segs)

detect_peaks <-
    function(time, signal, window=80000, pad=window/4, sRate=1e9/7682304, peak_limit=1.5,
             max_dist=60/220, omit_segments=NULL, ..., return_index=TRUE, adjust=TRUE, cores=1)
{
    if(!is.null(omit_segments)) {
        signal <- zero_segments(signal, omit_segments)
    }

    stopifnot(length(time) == length(signal))

    time <- convert_timestamp(time)

    # internal function to create non-overlapping windows with padding on each side
    window_info <- create_windows(length(signal), window=window, pad=pad)

    cores <- setup_cluster(cores)

    by_group_func <- function(i) {
        result <- rsleep_detect_rpeaks(signal[window_info$pre[i]:window_info$post[i]], sRate=sRate, ..., limit_factor=peak_limit, return_index=TRUE)

        result <- result + window_info$pre[i]-1
        result[result >= window_info$start[i] & window_info$end[i]]
    }

    result <- unlist( cluster_lapply(cores, seq_len(nrow(window_info)), by_group_func) )

    result <- sort(unique(result)) # this is the result if return_index = TRUE

    if(adjust) {  # adjust the peaks to hit the maxima
        result <- unique( adjust_peaks(result, signal) )
    }

    # throw out closely spaced peaks
    d <- as.numeric(diff(time[result]))
    if(any(d < max_dist)) {
        wh <- which(d < max_dist)

        # smoothed squared differences
        sqdiff <- diff(signal)^2
        smsqdiff <- (c(sqdiff,0,0,0) + c(0,sqdiff, 0, 0) + c(0,0,sqdiff,0))/3

        omit <- rep(-1, length(wh))
        for(i in seq_along(wh)) {
            peak1 <- result[wh[i]]
            peak2 <- result[wh[i]+1]
            omit[i] <- ifelse(smsqdiff[peak1] < smsqdiff[peak2], wh[i],  wh[i]+1)
        }

        result <- result[-omit]
    }

    if(!return_index) { # convert to times
        result <- time[result] # maybe need result + 1
    }

    result
}
