context("running_pvc_stats")

test_that("simple test of running_pvc_stats", {

    tz <- "America/Chicago"

    data(h10)
    peaks <- detect_peaks(h10$ecg)
    peak_stats <- calc_peak_stats(peaks, h10$ecg)
    pvc <- (peak_stats$RSdist > 6)

    h10$datetime <- convert_timestamp(h10$time, tz=tz)
    at <- seq(h10$datetime[1], max(h10$datetime), by=10)
    pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, at=at, tz=tz)

    expected <- structure(list(time = structure(c(1714447961.40073, 1714447966.39944,
                     1714447973.89942, 1714447981.38017), class = c("POSIXct", "POSIXt"),
                     tzone = tz),
                     pvc_percent = c(7.14285714285714, 8, 13.3333333333333,
                                     15.3846153846154),
                     hr = c(56.0001005810319, 60.0063475647658,
                            60.0156600806509, 62.5414485424771),
                     window_length = c(14.9999730587006, 24.9973554611206,
                                       29.9921720027924, 24.9434580802917)),
                     row.names = c(NA, -4L), class = c("pvc_stats", "data.frame"))

    expect_equal(pvc_stats, expected)


    # now test multicore
    skip_if(isnt_karl(), "this test only run locally")

    pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, at=at, cores=2, tz=tz)
    expect_equal(pvc_stats, expected)

    pvc_stats <- running_pvc_stats(h10$time, peaks, pvc, window=30, at=at, cores=4, tz=tz)
    expect_equal(pvc_stats, expected)

})
