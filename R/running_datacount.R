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
#' @param return_prop If true, return the expected proportion of
#'     observed data points within each window. Values will be
#'     slightly <1 even with complete data, due to discrete nature of
#'     time points.
#'
#' @param sRate Signal rate, as expected number of data points per
#'     second; needed if `return_prop=TRUE`
#'
#' @return Vector of same length as `times`, with the number of data points in a sliding window
#' centered at each time point.
#'
#' @details If `n_at` and `at` are both missing, we calculate amount of data at `times`.
#' If `at` is missing but `n_at` is provided, we use `n_at` equally-spaced times across
#' the observed range.
#'
#' @importFrom broman runningmean timeplot
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' polar_h10$datetime <- convert_timestamp(polar_h10$time)
#' n_pts <- running_datacount(polar_h10$datetime)
#'
#' broman::timeplot(polar_h10$datetime, n_pts,
#'                  xlab="Time", ylab="No. data points")

running_datacount <-
    function(times, window=1, at=NULL, n_at=NULL, tz=Sys.timezone(),
             return_prop=FALSE, sRate=1e9/7682304)
{

    times <- convert_timestamp(times, tz=tz)

    if(is.null(at)) {
        if(is.null(n_at)) at <- times
        else at <- seq(min(times), max(times), length.out=n_at)
    }
    else at <- convert_timestamp(at, tz=tz)

    result <- broman::runningmean(times, rep(1, length(times)), at=at, window=window, "sum")
    result[is.na(result)] <- 0

    if(return_prop) {
        # find lengths of windows in seconds that are within data range
        start <- at - window/2
        end <- at + window/2

        window_len <- rep(window, length(at))

        # window outside the range
        wh <- (end < min(times) | start > max(times))
        window_len[wh] <- 0

        # window overlapping min(times)
        wh <- (start < min(times) & end < max(times))
        window_len[wh] <- (end[wh] - min(times))

        # window overlapping max(times)
        wh <- (start < max(times) & end > max(times))
        window_len[wh] <- (max(times) - start[wh])

        result[window_len > 0] <- result[window_len > 0] / (sRate * window_len[window_len>0]+1)
        result[window_len == 0] <- 0 # use 0 rather than NA for 0/0
    }

    result
}
