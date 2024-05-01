context("detect_peaks")

test_that("simple test of detect_peaks", {

    data(h10)

    # don't adjust peak positions
    expect_equal(detect_peaks(h10$ecg, adjust=FALSE),
                 rsleep::detect_rpeaks(h10$ecg, sRate=1e9/7682304, return_index=TRUE))

    expect_equal(detect_peaks(h10$ecg, window=2000, adjust=FALSE),
                 detect_peaks(h10$ecg, adjust=FALSE))

    # don't adjust peak positions; return times not indices
    expect_equal(detect_peaks(h10$ecg, return_index=FALSE, adjust=FALSE),
                 rsleep::detect_rpeaks(h10$ecg, sRate=1e9/7682304, return_index=FALSE))

    expect_equal(detect_peaks(h10$ecg, window=2000, return_index=FALSE, adjust=FALSE),
                 detect_peaks(h10$ecg, return_index=FALSE, adjust=FALSE))


    # adjust peak positions
    expect_equal(detect_peaks(h10$ecg, window=2000),
                 detect_peaks(h10$ecg))

    expect_equal(detect_peaks(h10$ecg, window=2000, return_index=FALSE),
                 detect_peaks(h10$ecg, return_index=FALSE))

})
