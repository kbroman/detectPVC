context("adjust_peaks")

test_that("simple test of adjust_peaks", {

    data(polar_h10)

    time <- polar_h10$time[1:400]
    ecg <- polar_h10$ecg[1:400]

    peaks <- detect_peaks(time, ecg, adjust=FALSE)

    peaks_adj <- adjust_peaks(peaks, ecg)

    expect_equal(peaks_adj, detect_peaks(time, ecg))
    expect_equal(peaks_adj, c(78, 132, 250, 334, 391))

})
