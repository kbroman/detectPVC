context("calc_peak_stats")

test_that("simple test of calc_peak_stats", {

    data(polar_h10)

    ecg <- polar_h10$ecg[401:800]

    peaks <- detect_peaks(ecg)
    peakstats <- calc_peak_stats(peaks, ecg)

    expected <- structure(list(pmax = c(0.510, 0.520, 0.802, 0.368),
                               pmin = c(-0.664, -0.741, -0.517, -0.609),
                               Tmax = c(0.156, 0.802, -0.036, 0.132),
                               leftRR = c(NA, 85, 56, 114),
                               rightRR = c(85, 56, 114, NA),
                               RRratio = c(NA, 1.51785714285714, 0.491228070175439, NA),
                               RSdist = c(4, 4, 7, 5)),
                          row.names = c("105", "190", "246", "360"), class = "data.frame")

    expect_equal(peakstats, expected)

})
