#' Convert date/time stamp for Polar H10
#'
#' Convert the date/time stamp in the Polar H10 data to two columns: date/time and fractional seconds
#'
#' @param timestamp A vector of "epoch"-based integers as returned in the Polar H10 data
#'
#' @param tz Timezone
#'
#' @return A vector of Date objects
#'
#' @importFrom lubridate as_datetime
#'
#' @export
#'
#' @examples
#' data(h10)
#' ts <- convert_timestamp(h10[,1])
#' h10 <- cbind(h10, datetime=ts)

convert_timestamp <-
    function(timestamp, tz=Sys.timezone())
{
    lubridate::as_datetime(timestamp/1e9, tz=tz)
}
