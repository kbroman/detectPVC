context("find_bad_segments")

test_that("simple test of find_bad_segments", {

    data(polar_h10)

    badseg <- find_bad_segments(polar_h10$time, polar_h10$ecg)
    expected <- polar_h10$ecg
    expected[5874:6768] <- 0
    expect_equal(zero_segments(polar_h10$ecg, badseg), expected)

    polar_h10 <- polar_h10[get_time_interval(polar_h10$time, "2024-05-05 09:51", "2024-05-05 10:00"),]
    badseg <- find_bad_segments(polar_h10$time, polar_h10$ecg)
    expect_equal(zero_segments(polar_h10$ecg, badseg), polar_h10$ecg)

})
