context("get_time_interval")

test_that("simple tests of get_time_interval", {

    data(polar_h10)

    tz <- "America/Chicago"

    time_int1 <- get_time_interval(polar_h10$time, start="2024-05-05 09:53:00", length=4, tz=tz)
    time_int2 <- get_time_interval(polar_h10$time, start="2024-05-05 09:53:00", end="2024-05-05 09:53:04", tz=tz)
    time_int3 <- get_time_interval(polar_h10$time, end="2024-05-05 09:53:04", length=4, tz=tz)

    expect_equal(time_int1, time_int2)
    expect_equal(time_int1, time_int3)

    expect_equal( time_int1, 23431:23950 )

})
