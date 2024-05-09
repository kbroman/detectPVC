#' Get indexes for a interval in time
#'
#' For a vector of times, get the indexes that correspond to a specified interval
#'
#' @param times A vector of times, either as integers (Polar H10 times in 1e-9 seconds),
#' as datetimes, or as character strings of the form "2024-05-02 08:56" or
#' "2024-05-02 08:56:00"
#'
#' @param start Start time of interval
#'
#' @param end End time of interval
#'
#' @param length Length of interval (in seconds)
#'
#' @param tz Time zone
#'
#' @return A vector of numeric indexes, for values in `times` that are within the specified interval.
#'
#' @details Two of `start`, `end`, and `length`.
#'
#' @seealso [convert_timestamp()]
#'
#' @importFrom lubridate %within%
#' @importFrom lubridate interval
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' time_int1 <- get_time_interval(polar_h10$time, start="2024-05-05 09:52:00", length=4)
#' time_int2 <- get_time_interval(polar_h10$time, start="2024-05-05 09:52:00", end="2024-05-05 09:54:20")

get_time_interval <-
    function(times, start=NULL, end=NULL, length=NULL, tz=Sys.timezone())
{
    if(is.null(start) + is.null(end) + is.null(length) != 1) {
        stop("Must specify exactly two of start, end, length")
    }

    times <- convert_timestamp(times, tz=tz)
    if(!is.null(start)) start <- convert_timestamp(start, tz=tz)
    if(!is.null(end)) end <- convert_timestamp(end, tz=tz)

    if(is.null(end)) { # calculate end from start and length
        end <- start + length
    }
    if(is.null(start)) { # calculate start from end and length
        start <- end - length
    }

    which( times %within% lubridate::interval(start, end) )
}
