context("find_bad_segments")

test_that("simple test of find_bad_segments", {

    data(h10)

    expect_equal(find_bad_segments(h10$time, h10$ecg),
                 structure(list(start = numeric(0), end = numeric(0)),
                           class = "data.frame", row.names = integer(0)))

    h10$ecg[3896:3931] <- 2.5
    expect_equal(find_bad_segments(h10$time, h10$ecg),
                 structure(list(start = 3490, end = 4339),
                           class = "data.frame", row.names = c(NA, -1L)))

})
