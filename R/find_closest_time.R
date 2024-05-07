#' Find closest time
#'
#' Find the closest time in a vector of values to a specified target time
#'
#' @param target The target time, for which we want to find the closest value
#'
#' @param times A vector of times
#'
#' @param tz Time zone
#'
#' @return Numeric index of the closest time
#'
#' @export
#'
#' @examples
#' times <- seq(convert_timestamp("2024-05-01 11:00"),
#'              convert_timestamp("2024-05-01 14:00"), length=300)
#' find_closest_time("2024-05-01 12:00", times)

find_closest_time <-
    function(target, times, tz=Sys.timezone())
{
    target <- convert_timestamp(target, tz=tz)
    times <- convert_timestamp(times, tz=tz)

    which.min(abs(target - times))
}
