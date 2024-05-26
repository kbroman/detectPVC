context("most common date")

test_that("simple test of most_common_date", {

    data(polar_h10)

    expect_equal(most_common_date(polar_h10$time), "2024-05-05")

    z <- rep(c("2024-05-05 09:00", "2024-05-07 12:01", "2024-05-21 18:37"), c(20,5,30))
    expect_equal(most_common_date(z), "2024-05-21")

    z <- rep(c("2024-05-05 09:00", "2024-05-07 12:01", "2024-05-21 18:37"), c(20,35,30))
    expect_equal(most_common_date(z), "2024-05-07")

    z <- rep(c("2024-05-05 09:00", "2024-05-07 12:01", "2024-05-21 18:37"), c(40,35,30))
    expect_equal(most_common_date(z), "2024-05-05")

})
