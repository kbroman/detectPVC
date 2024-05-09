context("find_pvc_pattern")

test_that("simple test of find_pvc_pattern", {

    data(polar_h10)
    bad_segs <- find_bad_segments(polar_h10$time, polar_h10$ecg)
    peaks <- detect_peaks(polar_h10$ecg, omit_segments=bad_segs)
    peak_stats <- calc_peak_stats(polar_h10$time, polar_h10$ecg, peaks, omit_segments=bad_segs)
    pvc <- (peak_stats$RStime > 50)

    trigeminy <- find_pvc_pattern(polar_h10$time, peaks, pvc, pattern="(NNP)+",
                                  min_length=106, tz="America/Chicago")

    expected <- structure(list(start = c(8238, 33586),
                               end = c(18706, 44111),
                               n_beats = c(117L, 108L)),
                          class = "data.frame", row.names = c(NA, -2L))

    expect_equal(trigeminy, expected)

    bigeminy <- find_pvc_pattern(polar_h10$time, peaks, pvc, pattern="(NP)+",
                                 min_length=20)

    expected <- structure(list(start = c(29449, 50820),
                               end = c(33189, 52889),
                               n_beats = c(40L, 22L)),
                          class = "data.frame", row.names = c(NA, -2L))

    expect_equal(bigeminy, expected)


    couplets <- find_pvc_pattern(polar_h10$time, peaks, pvc, pattern="PP+")
    expected <- structure(list(start = 52889,
                               end = 52943,
                               n_beats = 2L),
                          class = "data.frame", row.names = c(NA, -1L))
    expect_equal(couplets, expected)

    triplets <- find_pvc_pattern(polar_h10$time, peaks, pvc, pattern="PP+", min_length=3)
    empty <- structure(list(start = numeric(0),
                            end = numeric(0),
                            n_beats = numeric(0)),
                       class = "data.frame", row.names = integer(0))
    expect_equal(triplets, empty)

    normal <- find_pvc_pattern(polar_h10$time, peaks, pvc, pattern="N+", min_length=17)
    expected <- structure(list(start = c(18851, 63863),
                               end = c(20463, 65622),
                               n_beats = c(17L, 17L)),
                          class = "data.frame", row.names = c(NA,-2L))
    expect_equal(normal, expected)

})
