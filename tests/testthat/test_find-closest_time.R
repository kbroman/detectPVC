context("find_closest_time")

test_that("simple test of find_closest_time", {

    times <- seq(convert_timestamp("2024-05-01 11:00"),
                 convert_timestamp("2024-05-01 14:00"), length=300)
    expect_equal(find_closest_time("2024-05-01 12:00", times), 101)

    expect_equal(find_closest_time("2024-04-29 22:33:00", h10$time), 3397)

})
