context("calc_peak_stats")

test_that("simple test of calc_peak_stats", {

    data(h10)

    ecg <- h10$ecg[1:400]

    peaks <- detect_peaks(ecg)
    peakstats <- calc_peak_stats(peaks, ecg)

    expected <- structure(list(pmax = c(0.396, 0.475, 0.9),
                               pmin = c(-0.591, -0.62, -0.406),
                               Tmax = c(0.178, 0.168, -0.002),
                               leftRR = c(NA, 131, 68),
                               rightRR = c(131, 68, NA),
                               RRratio = c(NA, 1.92647058823529, NA)),
                          row.names = c("121", "252", "320"), class = "data.frame")

    expect_equal(peakstats, expected)


    # slightly different results if you truncate ecg at 320
    ecg <- ecg[1:320]
    expected[3,2] <- -0.147
    expected[3,3] <- NA
    expected[3,4] <- expected[2,5] <- 68
    expect_equal(peaks, detect_peaks(ecg))
    expect_equal(calc_peak_stats(peaks, ecg), expected)

})
