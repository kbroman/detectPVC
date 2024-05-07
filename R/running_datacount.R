#' Get running count of amount of data
#'
#' Get running sum of number of observed data points in sliding windows
#'
#' @param times Vector of times
#'
#' @param window Length of sliding window in seconds
#'
#' @param at Times at which to calculate the amount of data
#'
#' @param n_at Number of times at which to calculate the amount of data
#'
#' @param tz Time zone, to convert `times` using [convert_timestamp()]
#'
#' @return Vector of same length as `times`, with the number of data points in a sliding window
#' centered at each time point.
#'
#' @details If `n_at` and `at` are both missing, we calculate amount of data at `times`.
#' If `at` is missing but `n_at` is provided, we use `n_at` equally-spaced times across
#' the observed range.
#'
#' @importFrom broman runningmean time_axis
#'
#' @export
#'
#' @examples
#' data(h10)
#' h10$datetime <- convert_timestamp(h10$time)
#' n_pts <- running_datacount(h10$time)
#'
#' xax <- broman::time_axis(h10$datetime)
#' broman::grayplot(h10$datetime, n_pts, xat=NA, vlines=xax$x,
#'                  xlab="Time (seconds)", ylab="No. data points")
#' axis(side=1, at=xax$x, labels=xax$labels, mgp=c(2.1, 0.5, 0), tick=FALSE)

running_datacount <-
    function(times, window=1, at=NULL, n_at=NULL, tz=Sys.timezone())
{

    times <- convert_timestamp(times, tz=tz)

    if(is.null(at)) {
        if(is.null(n_at)) at <- times
        else at <- seq(min(times), max(times), length.out=n_at)
    }
    else at <- convert_timestamp(at, tz=tz)

    broman::runningmean(times, rep(1, length(times)), at=at, window=window, "sum")
}