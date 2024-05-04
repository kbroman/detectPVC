context("detect_peaks")

test_that("simple test of detect_peaks", {

    data(h10)

    # don't adjust peak positions
    expect_equal(detect_peaks(h10$ecg, adjust=FALSE),
                 rsleep_detect_rpeaks(h10$ecg, sRate=1e9/7682304, limit_factor=1.5, return_index=TRUE))

    expect_equal(detect_peaks(h10$ecg, window=2000, adjust=FALSE),
                 detect_peaks(h10$ecg, adjust=FALSE))

    # don't adjust peak positions; return times not indices
    expect_equal(detect_peaks(h10$ecg, return_index=FALSE, adjust=FALSE),
                 rsleep_detect_rpeaks(h10$ecg, sRate=1e9/7682304, limit_factor=1.5, return_index=FALSE))

    expect_equal(detect_peaks(h10$ecg, window=2000, return_index=FALSE, adjust=FALSE),
                 detect_peaks(h10$ecg, return_index=FALSE, adjust=FALSE))


    # adjust peak positions
    expect_equal(detect_peaks(h10$ecg, window=2000),
                 detect_peaks(h10$ecg))

    expect_equal(detect_peaks(h10$ecg, window=2000, return_index=FALSE),
                 detect_peaks(h10$ecg, return_index=FALSE))

    # detect_peaks is pretty robust to the choice of peak_limit
    expect_equal(detect_peaks(h10$ecg, peak_limit=1.5),
                 detect_peaks(h10$ecg, peak_limit=3))

})


test_that("detect_peaks works with multiple cores", {

    skip_if(isnt_karl(), "this test only run locally")

    expect_equal(detect_peaks(h10$ecg),
                 detect_peaks(h10$ecg, window=1000))

    expect_equal(detect_peaks(h10$ecg),
                 detect_peaks(h10$ecg, window=1000, cores=4))

})
