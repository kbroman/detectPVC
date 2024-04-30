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
#' peaks <- find_rpeaks_sliding(h10$ecg)

find_rpeaks_sliding <-
    function(signal, window=2.5e5, sRate=1e9/7682304,
             ..., return_index=TRUE, adjust=TRUE)
{
    peaks <- rsleep::detect_rpeaks(signal, sRate=sRate, ..., return_index=return_index)

    peaks
}
