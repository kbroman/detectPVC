#' Detect R peaks in a raw ECG signal, using a sliding window
#'
#' Detect R peaks in a raw ECG signal, using [rsleep::detect_rpeaks()] but using a sliding window.
#'
#' @param signal Numerical vector of ECG signal
#'
#' @param window Integer indicating the number of values to consider at one time
#'
#' @param pad Integer indicating the number of values to consider on each side, surrounding the window
#'
#' @param sRate ECG signal rate
#'
#' @param ... Passed to [rsleep::detect_rpeaks()]
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
#' @importFrom rsleep detect_rpeaks
#' @importFrom parallel detectCores stopCluster makeCluster parLapply mclapply
#'
#' @seealso [adjust_peaks()]
#'
#'
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_peaks(h10$ecg)

detect_peaks <-
    function(signal, window=80000, pad=window/4, sRate=1e9/7682304,
             ..., return_index=TRUE, adjust=TRUE, cores=1)
{
    # internal function to create non-overlapping windows with padding on each side
    window_info <- create_windows(length(signal), window=window, pad=pad)

    cores <- setup_cluster(cores)

    by_group_func <- function(i) {
        result <- rsleep::detect_rpeaks(signal[window_info$pre[i]:window_info$post[i]], sRate=sRate, ..., return_index=TRUE)

        result <- result + window_info$pre[i]-1
        result[result >= window_info$start[i] & window_info$end[i]]
    }

    result <- unlist( cluster_lapply(cores, seq_len(nrow(window_info)), by_group_func) )

    result <- sort(unique(result)) # this is the result if return_index = TRUE

    if(adjust) {  # adjust the peaks to hit the maxima
        result <- unique( adjust_peaks(result, signal) )
    }

    if(!return_index) { # convert to times
        times <- seq(0, by=1/sRate, length.out = length(signal))
        result <- times[result + 1] # not sure why the +1, but...
    }

    result
}