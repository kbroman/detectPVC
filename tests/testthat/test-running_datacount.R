context("running_datacount")

test_that("simple test of running_datacount", {

    data(h10)
    n_pts <- running_datacount(h10$time)
    expect_equal(n_pts, c(66:130, rep(131, 5070), 130:66))

    w <- seq(1, by=10, length=100)
    n_pts_sub <- running_datacount(h10$time, at=h10$time[w])
    expect_equal(n_pts_sub, n_pts[w])

    n_pts2 <- running_datacount(h10$time, n_at=101)
    expect_equal(n_pts2, c(66, 118, rep(131, 5), rep(130, 87), rep(131, 5), 118, 66))

})
