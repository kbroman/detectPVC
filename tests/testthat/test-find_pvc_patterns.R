context("find_pvc_pattern")

test_that("simple test of find_pvc_pattern", {

    data(h10)
    peaks <- detect_peaks(h10$ecg)
    peak_stats <- calc_peak_stats(peaks, h10$ecg)
    pvc <- (peak_stats$RSdist > 6)

    trigeminy <- find_pvc_pattern(h10$time, peaks, pvc, pattern="(NNP)+",
                                  tz="America/Chicago")

    expected <- structure(list(start = c(121, 2504, 3486, 4102),
                               end = c(320, 2693, 3682, 4287),
                               n_beats = c(3L, 3L, 3L, 3L)),
                          class = "data.frame", row.names = c(NA, -4L))

    expect_equal(trigeminy, expected)

    bigeminy <- find_pvc_pattern(h10$time, peaks, pvc, pattern="(NP)+",
                                 min_length=4)

    expected <- structure(list(start = 3616,
                               end = 3928,
                               n_beats = 4L),
                          class = "data.frame", row.names = c(NA, -1L))

    expect_equal(bigeminy, expected)


    couplets <- find_pvc_pattern(h10$time, peaks, pvc, pattern="PP+")
    triplets <- find_pvc_pattern(h10$time, peaks, pvc, pattern="PP+", min_length=3)
    empty <- structure(list(start = numeric(0),
                            end = numeric(0),
                            n_beats = numeric(0)),
                       class = "data.frame", row.names = integer(0))
    expect_equal(couplets, empty)
    expect_equal(triplets, empty)

})
