#' Example Polar H10 data
#'
#' Example data from a Polar H10 chest-strap heart rate sensor
#'
#' @docType data
#'
#' @usage data(h10)
#'
#' @format A data frame with four columns: time (an epoch-based
#'     integer time stamp in 1e-9 seconds), ecg (the ECG values), hr
#'     (estimated heart rate in beats per minute), and rr (estimated
#'     R-R interval in milliseconds).
#'
#' @source Personal data derived from a Polar H10 monitor and extracted using the
#' [ECGLogger](https://www.ecglogger.com) app.
#'
#' @keywords datasets
#'
#' @examples
#' data(h10)
"h10"
