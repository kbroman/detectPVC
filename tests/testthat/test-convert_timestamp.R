context("convert_timestamp")

test_that("simple tests of convert_timestamp", {

    data(h10)

    tz <- "America/Chicago"

    expect_equal( convert_timestamp(h10$time[1], tz=tz),
                  lubridate::ymd_hms("2024-04-29 22:32:33.900752544", tz=tz))

    expect_equal( as.numeric(convert_timestamp("2024-04-29 22:32:33.900752544", tz=tz)),
                  h10$time[1]/1e9)

    expect_equal( convert_timestamp("2024-04-29 22:32", tz=tz),
                  lubridate::ymd_hm("2024-04-29 22:32", tz=tz))

    expect_equal( convert_timestamp(lubridate::ymd_hm("2024-04-29 22:32", tz=tz), tz=tz),
                  lubridate::ymd_hm("2024-04-29 22:32", tz=tz))

    expect_equal( convert_timestamp("2024-04-29 22:32:25.4", tz=tz),
                  lubridate::ymd_hms("2024-04-29 22:32:25.4", tz=tz))

    expect_equal( convert_timestamp(lubridate::ymd_hms("2024-04-29 22:32:25.4", tz=tz), tz=tz),
                  lubridate::ymd_hms("2024-04-29 22:32:25.4", tz=tz))


})
