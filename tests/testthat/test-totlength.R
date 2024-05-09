context("detect_peaks")

test_that("simple test of detect_peaks", {

    data(polar_h10)

    badsegs_i <- find_bad_segments(polar_h10$time, polar_h10$ecg)
    expect_equal(totlength(badsegs_i, polar_h10$time), 6.86820507049561)

    badsegs_t <- find_bad_segments(polar_h10$time, polar_h10$ecg, return_index=FALSE)
    expect_equal(totlength(badsegs_i, polar_h10$time), 6.86820507049561)



    polar_h10 <- polar_h10[get_time_interval(polar_h10$time, "2024-05-05 09:51", "2024-05-05 10:00"),]

    badsegs_i <- find_bad_segments(polar_h10$time, polar_h10$ecg)
    expect_equal(totlength(badsegs_i, polar_h10$time), 0)

    badsegs_t <- find_bad_segments(polar_h10$time, polar_h10$ecg, return_index=FALSE)
    expect_equal(totlength(badsegs_i, polar_h10$time), 0)

})
