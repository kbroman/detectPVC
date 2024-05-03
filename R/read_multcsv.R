#' Read multiple related CSV files
#'
#' Read multiple CSV files all with the same columns and rbind them together
#'
#' @param files Optional character vector of file names. If not provided, we use
#'     `list.files(dir)` to grab all CSV files in the directory `dir`.
#'
#' @param dir Optional subdirectory containing the files.
#'
#' @details At least one of `files` or `dir` must be provided.
#' The files should all have the same set of columns.
#'
#' @return A data.frame with the contents of all files row-binded together.
#'
#' @importFrom utils read.csv
#' @export

read_multcsv <-
    function(files=NULL, dir=".")
{
    if(is.null(files)) {
        if(is.null(dir)) stop("Provide at least one of files or dir")
        files <- list.files(dir, pattern="\\.csv")

        if(length(files) == 0) {
            stop("No CSV files found in directory ", dir)
        }

    }
    if(!is.null(dir)) files <- file.path(dir, files)

    do.call("rbind", lapply(files, read.csv))
}
