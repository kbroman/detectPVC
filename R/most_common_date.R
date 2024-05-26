#' Most common date in a vector of date/times
#'
#' Return the most common date in a vector of date/times
#'
#' @param times Vector of date/times
#'
#' @return The most common data, as a character string in ISO format like 2024-05-26
#'
#' @export
#'
#' @seealso [convert_timestamp()]

most_common_date <-
    function(times)
{
    times <- convert_timestamp(times)
    names(sort(table(format(times, format="%Y-%m-%d")), decreasing=TRUE))[1]
}
