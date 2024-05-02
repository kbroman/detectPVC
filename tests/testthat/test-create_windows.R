context("create_windows")

test_that("simple test of create_windows", {

    expect_equal(detectPVC:::create_windows(100000, 10000, 2000),
                 data.frame(pre=c(1,seq(3000, 93000, by=10000)),
                            start=c(1, seq(5000, 95000, by=10000)),
                            end=c(seq(4999, 94999, by=10000),100000),
                            post=c(seq(6999, 96999, by=10000),100000)))

})
