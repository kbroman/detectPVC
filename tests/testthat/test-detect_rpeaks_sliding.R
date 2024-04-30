context("detect_rpeaks_sliding")

test_that("simple test of detect_rpeaks_sliding", {

    data(h10)

    expect_equal(detect_rpeaks_sliding(h10$ecg),
                 rsleep::detect_rpeaks(h10$ecg, sRate=1e9/7682304, return_index=TRUE))

    expect_equal(detect_rpeaks_sliding(h10$ecg, window=2000),
                 detect_rpeaks_sliding(h10$ecg))

    expect_equal(detect_rpeaks_sliding(h10$ecg, return_index=FALSE),
                 rsleep::detect_rpeaks(h10$ecg, sRate=1e9/7682304, return_index=FALSE))

    expect_equal(detect_rpeaks_sliding(h10$ecg, window=2000, return_index=FALSE),
                 detect_rpeaks_sliding(h10$ecg, return_index=FALSE))


})
