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
#' @param omit_segments Segments to be ignored in analysis, as a data frame with
#' two columns: the start and end index for each interval to be ignored
#'
#' @param window Window in seconds to calculate the running statistics
#'
#' @param at Times at which to calculate the running statistics.
#'
#' @param n_at If `at` is not provided, we use this number of
#' equally-spaced values across the range of `times`.
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
#' pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, n_at=4)

running_pvc_stats <-
    function(times, peaks, pvc, omit_segments=NULL, window=480, at=NULL, n_at=240,
             cores=1, tz=Sys.timezone())
{
    stopifnot(all(peaks >= 1 & peaks <= length(times)))
    stopifnot(length(peaks) == length(pvc))
    cores <- setup_cluster(cores)

    times <- convert_timestamp(times, tz=tz)

    if(is.null(at)) {
        at <- seq(times[1], max(times), length=n_at)
    }

    calc_stats <-
        function(v)
    {
        center <- median(times[v])
        length <- diff(range(as.numeric(times[v])))
        beat_sum <- sum(peaks %in% v)
        pvc_sum <- sum(pvc[peaks %in% v])

        result <- data.frame(time=center,
                   pvc_percent=pvc_sum/beat_sum*100,
                   hr=beat_sum/length*60,
                   window_length=length)
        result
    }

    batch_func <-
        function(time)
    {
        v <- get_time_interval(times, time-window/2, time+window/2)

        if(!is.null(omit_segments)) {
            omit <- values_in_segments(v, omit_segments)

            if(any(omit)) { # look to see if we have a set of intervals
                v <- v[!omit]
                if(length(v)==0) return(NULL)
                dv <- diff(v)
                if(all(dv==1)) { # just one interval so return results
                    return(calc_stats(v))
                }

                # split into a set of intervals
                vspl <- split(v, cut(v, c(-Inf, v[which(dv > 1)]+0.5, Inf)))
                # toss small intervals
                vspl <- vspl[vapply(vspl, function(vv) diff(range(vv))>1, TRUE)]
                if(length(vspl)==0) return(NULL)
                else if(length(vspl)==1) return(calc_stats(vspl[[1]]))

                results <- do.call("rbind", lapply(vspl, calc_stats))

                # combine the results of the set of intervals
                center <- median(range(times[v]))
                lengths <- results$window_length
                beat_sums <- results$hr*lengths/60
                pvc_sums <- results$pvc_percent*beat_sums/100

                return(data.frame(time=center,
                                  pvc_percent=sum(pvc_sums)/sum(beat_sums)*100,
                                  hr=sum(beat_sums)/sum(lengths)*60,
                                  window_length=sum(lengths)))

            }
        }

        calc_stats(v)
    }

    result <- cluster_lapply(cores, at, batch_func)
    result <- do.call("rbind", result)
    class(result) <- c("pvc_stats", "data.frame")

    result
}
