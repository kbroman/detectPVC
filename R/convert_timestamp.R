#' Convert date/time stamp for Polar H10
#'
#' Convert the date/time stamp in the Polar H10 data to two columns: date/time and fractional seconds
#'
#' @param timestamp A vector of "epoch"-based integers as returned in the Polar H10 data.
#'    Alternatively, a vector of character strings in format like "2024-05-02 08:30:00"
#'    or just "2024-05-02 08:30"
#'
#' @param tz Timezone
#'
#' @return A vector of Date objects
#'
#' @importFrom lubridate as_datetime
#' @importFrom stringr str_count
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' polar_h10$datetime <- convert_timestamp(polar_h10$time)

convert_timestamp <-
    function(timestamp, tz=Sys.timezone())
{
    if(is.numeric(timestamp)) {
        return( lubridate::as_datetime(timestamp/1e9, tz=tz) )
    }

    else if(is.character(timestamp)) {
        if(all(stringr::str_count(timestamp, ":") == 1)) { # assume no seconds
            return( lubridate::ymd_hm(timestamp, tz=tz) )
        }
        else if(all(stringr::str_count(timestamp, ":") == 2)) { # assume has seconds
            return( lubridate::ymd_hms(timestamp, tz=tz) )
        }
        else {
            stop("Invalid time format")
        }
    }
    else if("POSIXct" %in% class(timestamp) || "POSIXt" %in% class(timestamp)) { # pass through
        return(timestamp)
    }
    else stop("timestamp must be integer, character, or datetime")
}
