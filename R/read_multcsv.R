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
#'
#' @param omit_incomplete If TRUE, omit any rows where the time stamp looks
#' to be incomplete (having < 19 characters)
#'
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
#' Files can be gzipped: If a file has name like 'file.csv.gz' it is
#' gunzipped to a temporary directory and then read.
#'
#' If `dont_modify=FALSE` and there is a column `time`, the rows are
#' reordered using this column and a `datetime` column is added,
#' converting `time` with [convert_timestamp()]
#'
#' @return A data.frame with the contents of all files row-binded together.
#'
#' @importFrom utils read.csv
#' @importFrom R.utils gunzip
#' @export

read_multcsv <-
    function(dir=".", files=NULL, dont_modify=FALSE, omit_incomplete=TRUE,
             tz=Sys.timezone(), cores=1)
{
    if(is.null(files)) {
        if(is.null(dir)) stop("Provide at least one of files or dir")
        files <- list.files(dir, pattern="\\.csv")

        if(length(files) == 0) {
            stop("No CSV files found in directory ", dir)
        }

    }
    if(!is.null(dir)) files <- file.path(dir, files)

    # omit files that are empty
    files <- files[file.size(files) > 0]
    if(length(files)==0) stop("No non-empty CSV files")

    cores <- setup_cluster(cores)

    result <- cluster_lapply(cores, files, read.csv_or_gz)

    result <- do.call("rbind", result)

    if(!dont_modify && "time" %in% colnames(result)) {
        result <- result[order(result$time), , drop=FALSE]
        result$datetime <- convert_timestamp(result$time, tz=tz)
    }

    if(omit_incomplete) { # drop rows with incomplete time stamps
        result <- result[nchar(result$time) >= 19, ]
    }

    result
}


read.csv_or_gz <-
    function(file, ...)
{
    if(grepl(".csv.gz$", file)) { # looks like gzipped csv
        # temp file to contain unzipped contents
        temp_file <- tempfile()

        # unzip file
        R.utils::gunzip(file, destname=temp_file, overwrite=TRUE, remove=FALSE)

        # on exit, remove the temp file
        on.exit(unlink(temp_file))

        file <- temp_file
    }

    # read the actual file
    return(read.csv(file, ...))
}
