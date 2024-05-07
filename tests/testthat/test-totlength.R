context("detect_peaks")

test_that("simple test of detect_peaks", {

    data(h10)
    expect_equal(totlength(find_bad_segments(h10$time, h10$ecg)), 0)

    h10$ecg[3896:3931] <- 2.5
    badsegs_i <- find_bad_segments(h10$time, h10$ecg)
    expect_equal(totlength(badsegs_i, h10$time), 6.52400422096252)

    badsegs_t <- find_bad_segments(h10$time, h10$ecg, return_index=FALSE)
    expect_equal(totlength(badsegs_i, h10$time), 6.52400422096252)

})
