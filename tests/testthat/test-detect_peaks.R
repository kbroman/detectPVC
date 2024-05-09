context("detect_peaks")

test_that("simple test of detect_peaks", {

    data(polar_h10)
    polar_h10 <- polar_h10[10001:20000,]

    # don't adjust peak positions
    expect_equal(detect_peaks(polar_h10$ecg, adjust=FALSE),
                 rsleep_detect_rpeaks(polar_h10$ecg, sRate=1e9/7682304, limit_factor=1.5, return_index=TRUE))

    expect_equal(detect_peaks(polar_h10$ecg, adjust=FALSE),
                 rsleep_detect_rpeaks(polar_h10$ecg, sRate=1e9/7682304, limit_factor=1.5, return_index=TRUE))

    # don't adjust peak positions; return times not indices
    expect_equal(detect_peaks(polar_h10$ecg, return_index=FALSE, adjust=FALSE),
                 rsleep_detect_rpeaks(polar_h10$ecg, sRate=1e9/7682304, limit_factor=1.5, return_index=FALSE))

})


test_that("detect_peaks works with multiple cores", {

    skip_if(isnt_karl(), "this test only run locally")

    expect_equal(detect_peaks(polar_h10$ecg),
                 detect_peaks(polar_h10$ecg, window=1000))

    expect_equal(detect_peaks(polar_h10$ecg),
                 detect_peaks(polar_h10$ecg, window=1000, cores=4))

})
