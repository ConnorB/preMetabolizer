test_that("even_timesteps rounds timestamps to the inferred interval", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2020-01-16 20:59", "2020-01-16 21:09", "2020-01-16 21:19"),
      tz = "UTC"
    ),
    do_mgl = c(6, 7, 8)
  )
  result <- even_timesteps(df)
  expect_equal(
    result$DateTime_UTC,
    as.POSIXct(
      c("2020-01-16 21:00", "2020-01-16 21:10", "2020-01-16 21:20"),
      tz = "UTC"
    )
  )
  expect_equal(result$do_mgl, c(6.1, 7.1, 8))
})

test_that("even_timesteps keeps on-grid timestamps and values unchanged", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "30 min",
      length.out = 6
    ),
    temp_C = c(10.1, 10.4, 10.5, 10.3, 10.2, 10.6)
  )
  result <- even_timesteps(df)
  expect_equal(result$DateTime_UTC, df$DateTime_UTC)
  expect_equal(result$temp_C, df$temp_C)
})

test_that("even_timesteps leaves NA in gaps wider than max_gap", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 5
    ),
    temp_C = c(1, 2, 3, 4, 5)
  )
  df_gap <- df[c(1, 2, 4, 5), , drop = FALSE]
  result <- even_timesteps(df_gap)
  expect_equal(nrow(result), 5)
  expect_equal(result$temp_C, c(1, 2, NA, 4, 5))
})

test_that("even_timesteps interpolates across gaps when max_gap = Inf", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 5
    ),
    temp_C = c(1, 2, 3, 4, 5)
  )
  df_gap <- df[c(1, 2, 4, 5), , drop = FALSE]
  result <- even_timesteps(df_gap, max_gap = Inf)
  expect_equal(result$temp_C, c(1, 2, 3, 4, 5))
})

test_that("even_timesteps interpolates POSIXct value columns", {
  df <- data.frame(
    utc = as.POSIXct(
      c("2024-01-01 00:59", "2024-01-01 01:09"),
      tz = "UTC"
    ),
    local = as.POSIXct(
      c("2024-01-01 06:59", "2024-01-01 07:09"),
      tz = "UTC"
    )
  )
  result <- even_timesteps(df, datetime_col = "utc")
  expect_equal(
    result$local,
    as.POSIXct(c("2024-01-01 07:00", "2024-01-01 07:09"), tz = "UTC")
  )
})

test_that("even_timesteps preserves POSIXct value column time zones", {
  df <- data.frame(
    utc = seq(
      as.POSIXct("2024-01-01 00:00", tz = "UTC"),
      by = "1 hour",
      length.out = 3
    ),
    local = seq(
      as.POSIXct("2023-12-31 17:00", tz = "America/Denver"),
      by = "1 hour",
      length.out = 3
    )
  )
  result <- even_timesteps(df, datetime_col = "utc")
  expect_equal(attr(result$local, "tzone"), "America/Denver")
  expect_equal(result$local, df$local)
})

test_that("even_timesteps fills non-numeric columns from nearest observation", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:59", "2024-01-01 01:09"),
      tz = "UTC"
    ),
    sonde = c("A", "B")
  )
  expect_snapshot(result <- even_timesteps(df))
  expect_equal(result$sonde, c("A", "B"))
})

test_that("even_timesteps respects nearest-fill max_gap boundary", {
  grid_time <- as.POSIXct("2024-01-01 01:00:00", tz = "UTC")

  df_inside <- data.frame(
    DateTime_UTC = grid_time + c(-239, 361, 961),
    sonde = c("A", NA, NA)
  )
  suppressWarnings(result_inside <- even_timesteps(df_inside, max_gap = 480))
  expect_equal(result_inside$sonde[1], "A")

  df_outside <- data.frame(
    DateTime_UTC = grid_time + c(-241, 359, 959),
    sonde = c("A", NA, NA)
  )
  suppressWarnings(result_outside <- even_timesteps(df_outside, max_gap = 480))
  expect_equal(result_outside$sonde[1], NA_character_)
})

test_that("even_timesteps keeps all-NA numeric columns NA", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 3
    ),
    temp_C = NA_real_
  )
  result <- even_timesteps(df)
  expect_equal(result$temp_C, c(NA_real_, NA_real_, NA_real_))
})

test_that("even_timesteps fills a single off-grid numeric value nearby", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:59", "2024-01-01 01:09", "2024-01-01 01:19"),
      tz = "UTC"
    ),
    temp_C = c(NA, 7, NA)
  )
  result <- even_timesteps(df)
  expect_equal(
    result$DateTime_UTC,
    as.POSIXct(
      c("2024-01-01 01:00", "2024-01-01 01:10", "2024-01-01 01:20"),
      tz = "UTC"
    )
  )
  expect_equal(result$temp_C, c(NA_real_, 7, NA_real_))
})

test_that("even_timesteps averages duplicate non-missing values at one time", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:00", "2024-01-01 00:00", "2024-01-01 00:10"),
      tz = "UTC"
    ),
    temp_C = c(1, 3, NA)
  )
  result <- even_timesteps(df)
  expect_equal(result$temp_C, c(2, NA))
})

test_that("even_timesteps works with multiple sites", {
  times <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    by = "1 hour",
    length.out = 4
  )
  df <- data.frame(
    DateTime_UTC = c(times, times[-3]),
    site = c(rep("A", 4), rep("B", 3)),
    temp_C = c(1:4, 10, 12, 16)
  )
  result <- even_timesteps(df, site_col = "site")
  expect_equal(result$site, c(rep("A", 4), rep("B", 4)))
  expect_equal(result$temp_C[result$site == "A"], c(1, 2, 3, 4))
  expect_equal(result$temp_C[result$site == "B"], c(10, 12, NA, 16))
})

test_that("even_timesteps snaps clock-jittered multi-site data", {
  df <- tibble::tibble(
    DateTime_UTC = c(
      as.POSIXct(
        c("2024-01-01 00:00:43", "2024-01-01 01:00:34", "2024-01-01 03:00:36"),
        tz = "UTC"
      ),
      as.POSIXct(
        c("2024-01-01 00:00:21", "2024-01-01 00:30:42", "2024-01-01 02:00:42"),
        tz = "UTC"
      )
    ),
    Site = c("A", "A", "A", "B", "B", "B"),
    temp_C = 1:6
  )

  result <- even_timesteps(df, site_col = "Site")

  expect_equal(
    result$DateTime_UTC[result$Site == "A"],
    as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 01:00:00",
        "2024-01-01 02:00:00",
        "2024-01-01 03:00:00"
      ),
      tz = "UTC"
    )
  )
  expect_equal(
    result$DateTime_UTC[result$Site == "B"],
    as.POSIXct(
      c(
        "2024-01-01 00:00:00",
        "2024-01-01 00:30:00",
        "2024-01-01 01:00:00",
        "2024-01-01 01:30:00",
        "2024-01-01 02:00:00"
      ),
      tz = "UTC"
    )
  )
  expect_equal(result$temp_C[result$Site == "A"][c(1, 3, 4)], c(1, NA, 3))
  expect_equal(
    result$temp_C[result$Site == "B"][c(1, 3, 4, 5)],
    c(4, NA, NA, 6)
  )
})

test_that("even_timesteps detects a datetime column with any name", {
  df <- data.frame(
    timestamp = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 5
    )
  )
  df_gap <- df[c(1, 2, 4, 5), , drop = FALSE]
  result <- even_timesteps(df_gap)
  expect_equal(nrow(result), 5)
})

test_that("even_timesteps preserves data frame and tibble inputs", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 3
    )
  )
  expect_s3_class(even_timesteps(df), "data.frame")
  expect_s3_class(even_timesteps(tibble::as_tibble(df)), "tbl_df")
})

test_that("even_timesteps handles intervals larger than lubridate unit bounds", {
  origin <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  steps <- c(90 * 60, 25 * 3600, 36 * 3600, 45 * 86400)

  for (step in steps) {
    df <- data.frame(
      DateTime_UTC = origin + step * 0:2,
      temp_C = 1:3
    )
    expect_no_error(result <- even_timesteps(df))
    expect_equal(result$DateTime_UTC, df$DateTime_UTC)
    expect_equal(result$temp_C, df$temp_C)
  }
})

test_that("even_timesteps tolerates sub-second timestamp jitter", {
  start <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  df <- data.frame(
    DateTime_UTC = start + c(0.02, 600.04, 1200.01, 1800.03),
    temp_C = 1:4
  )
  result <- even_timesteps(df)
  expect_equal(result$DateTime_UTC, start + 600 * 0:3)
  expect_equal(result$temp_C, 1:4)
})

test_that("even_timesteps preserves local daily grids across DST", {
  tz <- "America/Denver"
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c(
        "2024-03-09 00:00:00",
        "2024-03-10 00:00:00",
        "2024-03-11 00:00:00",
        "2024-03-12 00:00:00"
      ),
      tz = tz
    ),
    temp_C = 1:4
  )
  result <- even_timesteps(df)
  expect_equal(
    format(result$DateTime_UTC, "%Y-%m-%d %H:%M:%S", tz = tz),
    c(
      "2024-03-09 00:00:00",
      "2024-03-10 00:00:00",
      "2024-03-11 00:00:00",
      "2024-03-12 00:00:00"
    )
  )
  expect_equal(result$temp_C, 1:4)
})

test_that("even_timesteps reaches endpoints for non-divisor intervals", {
  origin <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  df <- data.frame(
    DateTime_UTC = origin + 7 * 60 * 0:10,
    temp_C = 0:10
  )
  result <- even_timesteps(df)
  expect_equal(tail(result$DateTime_UTC, 1), tail(df$DateTime_UTC, 1))
  expect_equal(result$temp_C, df$temp_C)
})

test_that("even_timesteps drops missing timestamps with a warning", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:00", NA, "2024-01-01 01:00"),
      tz = "UTC"
    ),
    temp_C = c(1, 99, 2)
  )
  expect_snapshot(result <- even_timesteps(df))
  expect_equal(result$temp_C, c(1, 2))
})

test_that("even_timesteps drops missing site values with a warning", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:00", "2024-01-01 00:30", "2024-01-01 01:00"),
      tz = "UTC"
    ),
    site = c("A", NA, "A"),
    temp_C = c(1, 99, 2)
  )
  expect_snapshot(result <- even_timesteps(df, site_col = "site"))
  expect_equal(result$site, c("A", "A"))
  expect_equal(result$temp_C, c(1, 2))
})

test_that("even_timesteps errors without a datetime column", {
  df <- data.frame(x = 1:3)
  expect_snapshot(error = TRUE, even_timesteps(df))
})

test_that("even_timesteps errors with multiple datetime columns", {
  df <- data.frame(
    start = as.POSIXct("2024-01-01", tz = "UTC"),
    end = as.POSIXct("2024-01-02", tz = "UTC")
  )
  expect_snapshot(error = TRUE, even_timesteps(df))
})

test_that("even_timesteps errors on missing explicit datetime column", {
  df <- data.frame(time = as.POSIXct("2024-01-01", tz = "UTC"))
  expect_snapshot(error = TRUE, even_timesteps(df, datetime_col = "missing"))
})

test_that("even_timesteps errors on non-data.frame input", {
  expect_snapshot(error = TRUE, even_timesteps(1:10))
})

test_that("even_timesteps errors on invalid max_gap", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 3
    )
  )
  expect_snapshot(error = TRUE, even_timesteps(df, max_gap = -1))
})

test_that("even_timesteps warns with a single timestamp", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct("2024-01-01", tz = "UTC"),
    temp_C = 1
  )
  expect_snapshot(result <- even_timesteps(df))
  expect_equal(nrow(result), 0)
})

test_that("even_timesteps drops underdetermined sites instead of raw rows", {
  df <- data.frame(
    DateTime_UTC = as.POSIXct(
      c("2024-01-01 00:00", "2024-01-01 01:00", "2024-01-01 05:07:33"),
      tz = "UTC"
    ),
    site = c("A", "A", "B"),
    temp_C = c(1, 2, 99)
  )
  expect_snapshot(result <- even_timesteps(df, site_col = "site"))
  expect_equal(result$site, c("A", "A"))
  expect_equal(
    result$DateTime_UTC,
    as.POSIXct(c("2024-01-01 00:00", "2024-01-01 01:00"), tz = "UTC")
  )
})

test_that("even_timesteps infers step from modal diff, not first diff", {
  # Regular hourly data with one near-duplicate at the start. A previous
  # implementation took the first diff and would have mis-inferred a 1-min
  # step. The modal diff (1 hour) is the correct step.
  base <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    by = "1 hour",
    length.out = 8
  )
  df <- data.frame(
    DateTime_UTC = c(base[1] + 60, base[-1])
  )
  result <- even_timesteps(df)
  # 1-hour step over an ~7-hour span should produce ~8 rows, not ~420.
  expect_lt(nrow(result), 20L)
})

test_that("even_timesteps deprecates loggerData argument", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 3
    )
  )
  expect_snapshot(invisible(even_timesteps(loggerData = df)))
})
