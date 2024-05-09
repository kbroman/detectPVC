context("running_datacount")

test_that("simple test of running_datacount", {

    data(polar_h10)
    polar_h10 <- polar_h10[1:200, ]

    n_pts <- running_datacount(polar_h10$time)
    expect_equal(n_pts, c(66:130, rep(131, 70), 130:66))

    w <- seq(1, by=10, length=20)
    n_pts_sub <- running_datacount(polar_h10$time, at=polar_h10$time[w])
    expect_equal(n_pts_sub, n_pts[w])

    n_pts2 <- running_datacount(polar_h10$time, n_at=21)
    expect_equal(n_pts2, c(66, 76, 85, 95, 105, 115, 125, rep(130, 7), 125, 115, 105, 95, 85, 76, 66))

})
