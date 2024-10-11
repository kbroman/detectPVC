context("calc_peak_stats")

test_that("simple test of calc_peak_stats", {

    data(polar_h10)

    polar_h10 <- polar_h10[401:800,]

    peaks <- detect_peaks(polar_h10$time, polar_h10$ecg)
    peakstats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks)

    expected <- structure(list(pmax = c(0.510, 0.520, 0.802, 0.368),
                               pmin = c(-0.664, -0.741, -0.517, -0.609),
                               Tmax = c(0.156, 0.802, -0.036, 0.132),
                               leftRR = c(NA, 0.653002500534058, 0.430203199386597, 0.875807762145996),
                               rightRR = c(0.653002500534058, 0.430203199386597, 0.875807762145996, NA),
                               RRratio = c(NA, 1.51789317574843, 0.49120733793506, NA),
                               RStime = c(30.7295322418213, 30.7285785675049, 53.7776947021484, 38.4125709533691)),
                          row.names = c("105", "190", "246", "360"), class = "data.frame")

    expect_equal(peakstats, expected)

})
