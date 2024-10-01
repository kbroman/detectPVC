#' Summarize output of plot_states
#'
#' Summarize output of plot_states, with amount of time in different PVC states
#'
#' @param object An object of class `"states"`, as output by [plot_states()]
#'
#' @return A data frame with the different possible PVC states
#'     (normal, bigeminy, trigeminy, other, omitted, and total time)
#'     and amount of time spent in each, as well as the percent of the
#'     overall time.
#'
#' @seealso [plot_states()]
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#'
#' st <- plot_states(polar_h10$time, peaks, pvc, omit_segments=bad_segs, xlim=c(50, 60), draw=FALSE)
#' summary(st)

summary_states <-
function(object)
{
    tot_time <- diff(as.numeric(attr(object, "time_range")))

    states <- c("N", "B", "T", "O")
    lengths <- as.numeric(object[,2]) - as.numeric(object[,1])
    tot_lengths <- tapply(lengths, factor(object[,3], states), sum)

    result <- data.frame(state=c(states, "NA", "total"),
                         time=c(tot_lengths, tot_time - sum(tot_lengths), tot_time))
    rownames(result) <- c("normal", "bigeminy", "trigeminy", "omitted", "other", "total")
    result <- result[c(1:3,5,4,6),] # put omitted second-to-last
    result <- cbind(result, percent=result$time/tot_time*100)

    class(result) <- c("summary.states", "data.frame")
    result
}


#' @rdname summary_states
#' @param ... Ignored
#' @export
summary.states <-
    function(object, ...)
{
    summary_states(object)
}

#' Print summary of states object
#'
#' Print summary of states object
#'
#' @param x Object of class `"summary.states"`, as produced by [summary_states()].
#' @param digits Number of digits in printing; passed to [base::print()]
#' @param ... Ignored
#'
#' @return Invisibly returns the input, `x`.
#'
#' @seealso [plot_states()], [summary_states()]
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#'
#' st <- plot_states(polar_h10$time, peaks, pvc, omit_segments=bad_segs, xlim=c(50, 60), draw=FALSE)
#' summary(st)

print.summary.states <-
    function(x, digits=3, ...)
{
    print(as.data.frame(x, digits=digits))

    invisible(x)
}
