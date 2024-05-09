context("find_closest_time")

test_that("simple test of find_closest_time", {

    tz <- "America/Chicago"

    times <- seq(convert_timestamp("2024-05-01 11:00", tz=tz),
                 convert_timestamp("2024-05-01 14:00", tz=tz), length=300)
    expect_equal(find_closest_time("2024-05-01 12:00", times, tz=tz), 101)

    expect_equal(find_closest_time("2024-05-05 09:52:24", polar_h10$time, tz=tz), 18744)

})
