context("running_datacount")

test_that("simple test of running_datacount", {

    data(h10)
    n_pts <- running_datacount(h10$time)
    expect_equal(n_pts, c(66:130, rep(131, 5070), 130:66))

    w <- seq(1, by=10, length=100)
    n_pts_sub <- running_datacount(h10$time, at=h10$time[w])
    expect_equal(n_pts_sub, n_pts[w])

})
