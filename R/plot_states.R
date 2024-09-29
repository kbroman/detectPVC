#' Plot PVC states
#'
#' In a sequence of ECG peaks classified as PVC or not, find patterns
#' normal, bigeminy, trigeminy, and make a plot of these
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
#' @param min_length Minimum length (as number of beats) for a pattern instance.
#'
#' @param rect_col Vector of four rectangle colors: normal, bigeminy, trigeminy, omitted
#'
#' @param tz Time zone for converting time stamps
#'
#' @param ... Passed to [base::plot()]
#'
#' @return Data frame with start and end times and type of state
#' (normal, bigeminy, trigeminy, omitted)
#'
#' @seealso [find_pvc_pattern()]
#'
#' @importFrom stats setNames
#' @importFrom graphics axis rect
#' @export
#'
#' @examples
#' data(polar_h10)
#' bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
#' peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
#' peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
#' pvc <- (peak_stats$RStime > 50)
#'
#' plot_states(polar_h10$time, peaks, pvc, omit_segments=bad_segs)

plot_states <-
    function(times, peaks, pvc, omit_segments=NULL,
             min_length=12,
             rect_col=c(blue="#0074d9", orange="#ff851b", green="#2ecc40", purple="#cc00dd"),
             tz=Sys.timezone(), ...)
{
    times <- convert_timestamp(times, tz=tz)
    stopifnot(all(peaks >= 1 & peaks <= length(times)))
    stopifnot(length(pvc) == length(peaks))

    if(length(rect_col) < 4) { # if missing rectangle colors, pad with white
        rect_col <- c(rect_col, rep("white", 4-length(rect_col)))
    }

    normal <- find_pvc_pattern(times, peaks, pvc, omit_segments, "N+",
                               min_length=min_length, return_index=FALSE, tz=tz)
    trig <- find_pvc_pattern(times, peaks, pvc, omit_segments, "(NNP)+",
                             min_length=min_length, return_index=FALSE, tz=tz)
    big <- find_pvc_pattern(times, peaks, pvc, omit_segments, "(NP)+",
                            min_length=min_length, return_index=FALSE, tz=tz)

    # convert omit_segments from indexes to times
    if(!is.null(omit_segments)) {
        omit_segments <- as.data.frame( lapply(omit_segments,
                                               function(index) times[index]) )
    }

    # combine into data frame
    segments <- rbind( data.frame(start=normal[,1], end=normal[,2], state="N"),
                     data.frame(start=trig[,1],   end=trig[,2], state="T"),
                     data.frame(start=big[,1],    end=big[,2], state="B"),
                     data.frame(start=omit_segments[,1], end=omit_segments[,2], state="O"))

    result <- segments <- segments[order(segments[,1]),]

    # range of times in data
    time_range <- range(times)

    # pad to start and end of those hours
    first <- paste0(format(time_range[1], format="%Y-%m-%d"),
                    " ",
                    format(time_range[1], format="%H"),
                    ":00:00")
    first <- convert_timestamp(first)

    last <- paste0(format(time_range[2], format="%Y-%m-%d"),
                    " ",
                    format(time_range[2], format="%H"),
                    ":59:59.99")
    last <- convert_timestamp(last)
    if(last < time_range[2]) last <- time_range[2] + 0.001

    # pad segments
    segments <- rbind(data.frame(start=first, end=segments$start[1], state="O"),
                      segments,
                      data.frame(start=segments$end[nrow(segments)], end=last, state="O"))

    time_range <- c(first, last)

    first_last_dates <- format(time_range, format="%Y-%m-%d")
    all_one_date <- (first_last_dates[1] == first_last_dates[2])

    hours <- as.numeric(format(time_range, format="%H"))
    hour_limits <- c(hours[1], hours[1] + floor(diff(as.numeric(time_range))/3600))
    hour_limits <- hour_limits + c(-0.5, 0.5)

    # need to pull off the hours from this
    # make a function that goes from date/time -> hour, min

    start_time <- as.numeric(segments$start[1])
    start_hr <- floor((as.numeric(segments$start) - start_time)/3600) + hours[1]
    end_hr <- floor((as.numeric(segments$end) - start_time)/3600) + hours[1]
    start_numer <- (as.numeric(segments$start) - start_time) / 3600
    start_min <- (start_numer - floor(start_numer))*60
    end_numer <- (as.numeric(segments$end) - start_time) / 3600
    end_min <- (end_numer - floor(end_numer))*60
    state <- segments$state

    # split those that span the hour
    wh <- which(start_hr != end_hr)

    if(length(wh) > 1) { # assume that they're all < 1 hour in length
        start_hr <- c(start_hr, start_hr[wh], start_hr[wh]+1)
        end_hr <- c(end_hr, start_hr[wh], start_hr[wh]+1)
        start_min <- c(start_min, start_min[wh], rep(0, length(wh)))
        end_min <- c(end_min, rep(60, length(wh)), end_min[wh])
        state <- c(state, state[wh], state[wh])

        start_hr <- start_hr[-wh]
        end_hr <- end_hr[-wh]
        start_min <- start_min[-wh]
        end_min <- end_min[-wh]
        state <- state[-wh]
    }

    segments <- data.frame(start_hr=start_hr, start_min=start_min,
                           end_hr=end_hr, end_min=end_min,
                           state=state)

    plot_states_internal <-
        function(xlab="Minute", ylab="Hour", xaxs="i", yaxs="i",
                 mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL, las=1,
                 ylim=hour_limits, ...)

        {
            if(is.null(mgp.x)) mgp.x <- mgp
            if(is.null(mgp.y)) mgp.y <- mgp

            plot(0,0,type="n", xlab=xlab, ylab=ylab, xaxs=xaxs, yaxs=yaxs,
                 xaxt="n", yaxt="n", xlim=c(0,60), ylim=ylim, las=las, ...)
            axis(side=1, at=seq(0, 60, by=10), mgp=mgp.x, tick=FALSE, las=las)


            if(all_one_date) axis(side=2, at=pretty(ylim), mgp=mgp.y, tick=FALSE, las=las)
            else {
                p <- pretty(ylim)
                axis(side=2, at=p, labels=p %% 24, mgp=mgp.y, tick=FALSE, las=las)
            }

            rect_col <- setNames(rect_col[1:4], c("N", "B", "T", "O"))
            segments$rect_col <- rect_col[segments$state]

            for(i in 1:nrow(segments)) {
                rect(segments[i,"start_min"], segments[i,"start_hr"]-0.5,
                     segments[i,"end_min"], segments[i,"start_hr"]+0.5,
                     lend=1, ljoin=1, col=segments[i,"rect_col"],
                     border=NA)

            }


        }

    plot_states_internal(...)

    rownames(result) <- 1:nrow(result)
    invisible(result)
}
