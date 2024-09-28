#' Find PVC patterns
#'
#' In a sequence of ECG peaks classified as PVC or not, find patterns
#' like bigeminy, trigeminy, couplets and triplets.
#'
#' @param times Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param peaks Vector of peak indices as integers from 1 to `length(times)`
#'
#' @param pvc Vector of boolean indicators of whether the peaks are PVC or not.
#' Should be the same length as `peaks`.
#'
#' @param omit_segments Segments to be ignored in analysis, as a data frame with
#' two columns: the start and end index for each interval to be ignored
#'
#' @param pattern The character string with the regular expression to look for,
#' with `N`=normal, `P`=PVC, so for example `"(NNP)+"` for trigeminy.
#'
#' @param min_length Minimum length (as number of beats) for a pattern instance.
#'
#' @param return_index If TRUE, return indexes to times, not the actual times.
#'
#' @param tz Time zone for converting time stamps
#'
#' @return Data frame with start and end times + length of match (as number of peaks)
#'
#' @importFrom stringr str_extract_all str_locate_all
#' @export
#'
#' @seealso [plot_states()]
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#'
#' trigeminy <- find_pvc_pattern(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                               pattern="(NNP)+")
#' bigeminy <- find_pvc_pattern(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                              pattern="(NP)+", min_length=4)
#' couplets <- find_pvc_pattern(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                              pattern="PP+")
#' triplets <- find_pvc_pattern(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                              pattern="PP+", min_length=3)
#' normal <- find_pvc_pattern(polar_h10$time, peaks, pvc, omit_segments=bad_segs,
#'                            pattern="N+", min_length=10)

find_pvc_pattern <-
    function(times, peaks, pvc, omit_segments=NULL,
             pattern, min_length=0, return_index=TRUE,
             tz=Sys.timezone())
{
    times <- convert_timestamp(times, tz=tz)

    stopifnot(all(peaks >= 1 & peaks <= length(times)))
    stopifnot(length(pvc) == length(peaks))

    # pvc to characters
    pvc <- c("N", "P")[pvc+1]

    # insert a fake peak in the middle of each bad segment
    if(!is.null(omit_segments)) {
        n_peaks <- length(peaks)
        n_seg <- nrow(omit_segments)
        peaks <- c(peaks, rep(0, n_seg))
        pvc <- c(pvc, rep("B", n_seg)) # B for bad
        if(!is.null(omit_segments)) {
            for(i in seq_len(nrow(omit_segments))) {
                peaks[n_peaks+i] <-  median(unlist(omit_segments[i,]))
            }
        }
        # reorder peaks
        o <- order(peaks)
        peaks <- peaks[o]
        pvc <- pvc[o]
    }

    str <- paste(pvc, collapse="")

    # find all instances of pattern + their locations
    match <- stringr::str_extract_all(str, pattern)[[1]]
    loc <- stringr::str_locate_all(str, pattern)[[1]] # 2-col matrix with start and end

    # omit shorter ones
    keep <- (nchar(match) >= min_length)
    match <- match[keep]
    loc <- loc[keep,,drop=FALSE]

    if(length(match)==0) {
        return(data.frame(start=numeric(0),
                          end=numeric(0),
                          n_beats=numeric(0)))
    }

    # convert loc to times
    result <- data.frame(start=peaks[loc[,1]],
                         end=peaks[loc[,2]],
                         n_beats=nchar(match))

    if(!return_index) {
        result$start <- times[result$start]
        result$end <- times[result$end]
    }

    result
}
