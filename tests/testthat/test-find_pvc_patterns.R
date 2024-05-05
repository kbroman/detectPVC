context("find_pvc_pattern")

test_that("simple test of find_pvc_pattern", {

    data(h10)
    peaks <- detect_peaks(h10$ecg)
    peak_stats <- calc_peak_stats(peaks, h10$ecg)
    pvc <- (peak_stats$RSdist > 6)

    pat <- find_pvc_pattern(h10$time, peaks, pvc, pattern="(NNP)+", tz="America/Chicago")

    expected <- structure(list(start = structure(c(1714447954.82288, 1714447973.13484,
                                                   1714447980.6809, 1714447985.41447),
                                                 class = c("POSIXct", "POSIXt"),
                                                 tzone = "America/Chicago"),
                               end = structure(c(1714447956.35207, 1714447974.58717,
                                                 1714447982.18704, 1714447986.83607),
                                               class = c("POSIXct","POSIXt"),
                                               tzone = "America/Chicago"),
                               n_beats = c(3L, 3L, 3L, 3L)),
                          class = "data.frame", row.names = c(NA, -4L))

    expect_equal(pat, expected)
})
