context("find_bad_segments")

test_that("simple test of find_bad_segments", {

    data(h10)

    badseg <- find_bad_segments(h10$time, h10$ecg)
    expect_equal(zero_segments(h10$ecg, badseg), h10$ecg)

    h10$ecg[3896:3931] <- 2.5
    badseg <- find_bad_segments(h10$time, h10$ecg)
    expected <- h10$ecg
    expected[3490:4339] <- 0

    expect_equal(zero_segments(h10$ecg, badseg), expected)

})
