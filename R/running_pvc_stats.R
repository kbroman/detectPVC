#' Calculate running PVC burden and HR
#'
#' Calculate running PVC burden percent and heart rate in sliding windows
#'
#' @param times Vector of times (integers as from Polar H10, datetimes, or character strings)
#'
#' @param peaks Vector of peak indices as integers from 1 to `length(times)`
#'
#' @param pvc Vector of boolean indicators of whether the peaks are PVC or not.
#' Should be the same length as `peaks`.
#'
#' @param window Window in seconds to calculate the running statistics
#'
#' @param at Times at which to calculate the running statistics. If not provided,
#' we use a 1 minute spacing across the range of `times`.
#'
#' @param cores Number of CPU cores to use, for parallel calculations.
#' (If `0`, use [parallel::detectCores()].)
#' Alternatively, this can be links to a set of cluster sockets, as
#' produced by [parallel::makeCluster()].
#'
#' @param tz Timezone (in converting times)
#'
#' @return Data frame with window centers, PVC percent, heart rate (in
#' beats per minute), and window length (in seconds). The window
#' centers may be different from `at` in cases where windows is
#' missing data.
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' data(h10)
#' peaks <- detect_peaks(h10$ecg)
#' peak_stats <- calc_peak_stats(peaks, h10$ecg)
#' pvc <- (peak_stats$RSdist > 6)
#'
#' h10$datetime <- convert_timestamp(h10$time)
#' at <- seq(h10$datetime[1], max(h10$datetime), by=10)
#' pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, at=at)

running_pvc_stats <-
    function(times, peaks, pvc, window=240, at=NULL, cores=1, tz=Sys.timezone())
{
    stopifnot(all(peaks >= 1 & peaks <= length(times)))
    stopifnot(length(peaks) == length(pvc))
    cores <- setup_cluster(cores)

    times <- convert_timestamp(times, tz=tz)

    if(is.null(at)) {
        at <- seq(times[1], max(times), by=60)
    }

    batch_func <-
        function(time)
    {
        v <- get_time_interval(times, time-window/2, time+window/2)
        center <- median(times[v])
        length <- diff(range(as.numeric(times[v])))
        beat_sum <- sum(peaks %in% v)
        pvc_sum <- sum(pvc[peaks %in% v])

        return(data.frame(time=center,
                          pvc_percent=pvc_sum/beat_sum*100,
                          hr=beat_sum/length*60,
                          window_length=length))
    }

    result <- cluster_lapply(cores, at, batch_func)
    result <- do.call("rbind", result)

    result
}
