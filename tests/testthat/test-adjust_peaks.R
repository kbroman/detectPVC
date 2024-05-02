context("adjust_peaks")

test_that("simple test of adjust_peaks", {

    data(h10)

    ecg <- h10$ecg[1:400]

    peaks <- detect_peaks(ecg, adjust=FALSE)

    peaks_adj <- adjust_peaks(peaks, ecg)

    expect_equal(peaks_adj, detect_peaks(ecg))
    expect_equal(peaks_adj, c(121, 252, 320))

})
