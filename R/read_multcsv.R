#' Read multiple related CSV files
#'
#' Read multiple CSV files all with the same columns and rbind them together
#'
#' @param dir Subdirectory containing the files.
#'
#' @param files Optional character vector of file names. If not provided, we use
#'     `list.files(dir)` to grab all CSV files in the directory `dir`.
#'
#' @param dont_modify If TRUE, just rbind the file contents together. If FALSE,
#' look for a `time` column and reorder the rows by that column, and then
#' add a converted `datetime` column using [convert_timestamp()].

#' @param tz Time zone, used if `dont_modify=FALSE` and there is a time
#' column to convert.
#'
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @details At least one of `files` or `dir` must be provided.
#' The files should all have the same set of columns.
#'
#' If `dont_modify=FALSE` and there is a column `time`, the rows are
#' reordered using this column and a `datetime` column is added,
#' converting `time` with [convert_timestamp()]
#'
#' @return A data.frame with the contents of all files row-binded together.
#'
#' @importFrom utils read.csv
#' @export

read_multcsv <-
    function(dir=".", files=NULL, dont_modify=FALSE, tz=Sys.timezone(), cores=1)
{
    if(is.null(files)) {
        if(is.null(dir)) stop("Provide at least one of files or dir")
        files <- list.files(dir, pattern="\\.csv")

        if(length(files) == 0) {
            stop("No CSV files found in directory ", dir)
        }

    }
    if(!is.null(dir)) files <- file.path(dir, files)

    cores <- setup_cluster(cores)

    result <- cluster_lapply(cores, files, read.csv)

    result <- do.call("rbind", result)

    if(!dont_modify && "time" %in% colnames(result)) {
        result <- result[order(result$time), , drop=FALSE]
        result$datetime <- convert_timestamp(result$time, tz=tz)
    }

    result
}
