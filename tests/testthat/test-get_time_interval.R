context("get_time_interval")

test_that("simple tests of get_time_interval", {

    data(h10)

    tz <- "America/Chicago"

    time_int1 <- get_time_interval(h10$time, start="2024-04-29 22:33:00", length=4, tz=tz)
    time_int2 <- get_time_interval(h10$time, start="2024-04-29 22:33:00", end="2024-04-29 22:33:04", tz=tz)
    time_int3 <- get_time_interval(h10$time, end="2024-04-29 22:33:04", length=4, tz=tz)

    expect_equal(time_int1, time_int2)
    expect_equal(time_int1, time_int3)

    expect_equal( which(time_int1), 3398:3917 )

})
