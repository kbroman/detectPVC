context("find_bad_segments")

test_that("simple test of find_bad_segments", {

    data(polar_h10)
    tz <- "America/Chicago"
    polar_h10$datetime <- convert_timestamp(polar_h10$time, tz=tz)

    expect_equal(find_bad_segments(polar_h10$time, polar_h10$ecg, tz=tz),
                 structure(list(start = 5874, end = 6768),
                           class = "data.frame", row.names = c(NA, -1L)))

    expect_equal(find_bad_segments(polar_h10$time, polar_h10$ecg, return_index=FALSE, tz=tz),
                 structure(list(start = polar_h10$datetime[5874], end = polar_h10$datetime[6768]),
                           class = "data.frame", row.names = c(NA, -1L)))

})
