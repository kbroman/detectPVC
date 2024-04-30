#' Detect R peaks in a raw ECG signal, using a sliding window
#'
#' Detect R peaks in a raw ECG signal, using [rsleep::detect_rpeaks()] but using a sliding window.
#'
#' @param signal Numerical vector of ECG signal
#'
#' @param window Integer indicating the number of values to consider at one time
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
#'
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_rpeaks_sliding(h10$ecg)

detect_rpeaks_sliding <-
    function(signal, window=2.5e5, sRate=1e9/7682304,
             ..., return_index=TRUE, adjust=TRUE)
{
    starts <- seq(1, length(signal), by=window/2)

    result <- NULL
    for(i in seq_along(starts)) {
        this_window <- starts[i] + 1:window - 1
        this_window <- this_window[this_window <= length(signal)]

        this_result <- rsleep::detect_rpeaks(signal[this_window], sRate=sRate, ..., return_index=TRUE)

        this_result <- this_result + starts[i]-1

        result <- c(result, this_result)
    }

    result <- sort(unique(result)) # this is the result if return_index = TRUE

    if(!return_index) { # convert to times
        times <- seq(0, by=1/sRate, length.out = length(signal))
        result <- times[result + 1] # not sure why the +1, but...
    }

    result
}
