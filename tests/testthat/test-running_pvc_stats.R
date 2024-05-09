context("running_pvc_stats")

test_that("simple test of running_pvc_stats", {

    tz <- "America/Chicago"

    data(polar_h10)
    polar_h10 <- polar_h10[20001:30000,]

    peaks <- detect_peaks(polar_h10$ecg)
    peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks)
    pvc <- (peak_stats$RStime > 50)

    polar_h10$datetime <- convert_timestamp(polar_h10$time, tz=tz)
    at <- seq(polar_h10$datetime[1], max(polar_h10$datetime), by=10)
    pvc_stats <- running_pvc_stats(polar_h10$time, peaks, pvc, window=30, at=at, tz=tz)

    expected <- structure(list(time = structure(c(1714920761.15457, 1714920766.15594, 1714920773.65418, 1714920783.65689,
                                                  1714920793.65964, 1714920803.65473, 1714920813.65757, 1714920819.56939),
                                                class = c("POSIXct", "POSIXt"), tzone = "America/Chicago"),
                               pvc_percent = c(28.5714285714286, 29.4117647058824, 26.8292682926829, 20.5128205128205,
                                               20, 27.5, 30, 32.1428571428571),
                               hr = c(84.0202924147167, 81.6027891578325, 82.019504130702, 78.0184472786859,
                                      80.0188910259948, 80.0186302327418, 80.0184355930316, 77.0249968002038),
                               window_length = c(14.9963772296906, 24.9991455078125, 29.9928660392761, 29.9929065704346,
                                                 29.9929175376892, 29.9930152893066, 29.9930882453918, 21.8111011981964)),
                          row.names = c(NA, -8L), class = c("pvc_stats", "data.frame"))

    expect_equal(pvc_stats, expected)


    # now test multicore
    skip_if(isnt_karl(), "this test only run locally")

    pvc_stats <- running_pvc_stats(polar_h10$time, peaks, pvc, window=30, at=at, cores=2, tz=tz)
    expect_equal(pvc_stats, expected)

    pvc_stats <- running_pvc_stats(polar_h10$time, peaks, pvc, window=30, at=at, cores=4, tz=tz)
    expect_equal(pvc_stats, expected)

})
