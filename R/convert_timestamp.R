#' Convert date/time stamp for Polar H10
#'
#' Convert the date/time stamp in the Polar H10 data to two columns: date/time and fractional seconds
#'
#' @param timestamp A vector of "epoch"-based integers as returned in the Polar H10 data
#'
#' @param tz Timezone
#'
#' @return A data.frame with two columns: a DateTime vector and a fraction seconds remainder
#'
#' @importFrom lubridate as_datetime
#'
#' @export
#'
#' @examples
#' data(h10)
#' ts <- convert_timestamp(h10[,1])
#' h10 <- cbind(h10, ts)

convert_timestamp <-
    function(timestamp, tz=Sys.timezone())
{
    datetime <- lubridate::as_datetime(timestamp/1e9, tz=tz)

    frac_sec <- timestamp/1e9 - as.integer(datetime)

    data.frame(datetime=datetime, frac_sec=frac_sec)
}
