#' Total length of a set of time intervals
#'
#' Total length of a set of time intervals, in seconds
#'
#' @param segments A data frame with two columns, the start and end of
#' a set of time intervals, either as integer indexes to a
#' provided vector of times, or as a set of date/times.
#'
#' @param times A vector of times, needed if `segments` is a set of
#' integer indexes
#'
#' @param tz Time zone, used to convert `times` with `convert_timestamp`
#'
#' @return Total length of the set of time segments, in seconds
#'
#' @export
#'
#' @examples
#' data(polar_h10)
#' badsegs_i <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' totlength(badsegs_i, polar_h10$time)
#'
#' badsegs_t <- find_bad_segments(polar_h10$time, polar_h10$ecg, return_index=FALSE)
#' totlength(badsegs_t)

totlength <-
    function(segments, times=NULL, tz=Sys.timezone())
{
    if(nrow(segments)==0) return(0)

    if(!("POSIXct" %in% class(segments[,1]) ||
         "POSIXt" %in% class(segments[,1]))) {
        if(is.null(times)) stop("times is NULL but is needed to convert segments to date/time")
        segments <- segs_index_to_time(segments, convert_timestamp(times, tz))
    }

    sum(as.numeric(segments[,2]) - as.numeric(segments[,1]))
}
