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
#' @importFrom rsleep detect_rpeaks
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
    function(signal, window=80000, pad=15000, sRate=1e9/7682304,
             ..., return_index=TRUE, adjust=TRUE)
{
    # internal function to create non-overlapping windows with padding on each side
    window_info <- create_windows(length(signal), window=window, pad=pad)

    result <- NULL
    for(i in seq_len(nrow(window_info))) {
        this_result <- rsleep::detect_rpeaks(signal[window_info$pre[i]:window_info$post[i]], sRate=sRate, ..., return_index=TRUE)

        this_result <- this_result + window_info$pre[i]-1
        this_result <- this_result[this_result >= window_info$start[i] & window_info$end[i]]

        result <- c(result, this_result)
    }

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
