context("find_bad_segments")

test_that("simple test of find_bad_segments", {

    data(h10)
    h10$datetime <- convert_timestamp(h10$time, "America/Chicago")

    expect_equal(find_bad_segments(h10$time, h10$ecg),
                 structure(list(start = numeric(0), end = numeric(0)),
                           class = "data.frame", row.names = integer(0)))

    h10$ecg[3896:3931] <- 2.5
    expect_equal(find_bad_segments(h10$time, h10$ecg),
                 structure(list(start = 3490, end = 4339),
                           class = "data.frame", row.names = c(NA, -1L)))

    expect_equal(find_bad_segments(h10$time, h10$ecg, return_index=FALSE),
                 structure(list(start = h10$datetime[3490], end = h10$datetime[4339]),
                           class = "data.frame", row.names = c(NA, -1L)))

})
