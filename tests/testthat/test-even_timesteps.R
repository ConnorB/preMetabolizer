test_that("even_timesteps fills gap in single-site data", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "1 hour",
      length.out = 5
    )
  )
  df_gap <- df[c(1, 2, 4, 5), , drop = FALSE]
  result <- even_timesteps(df_gap)
  expect_equal(nrow(result), 5)
  expect_s3_class(result, "data.frame")
})

test_that("even_timesteps preserves original timestamps", {
  df <- data.frame(
    DateTime_UTC = seq(
      as.POSIXct("2024-01-01", tz = "UTC"),
      by = "30 min",
      length.out = 6
    )
  )
  df_gap <- df[c(1, 2, 4, 5, 6), , drop = FALSE]
  result <- even_timesteps(df_gap)
  expect_equal(nrow(result), 6)
})

test_that("even_timesteps works with multiple sites", {
  times <- seq(
    as.POSIXct("2024-01-01", tz = "UTC"),
    by = "1 hour",
    length.out = 4
  )
  df <- data.frame(
    DateTime_UTC = c(times, times[-3]),
    site = c(rep("A", 4), rep("B", 3))
  )
  result <- even_timesteps(df, site_col = "site")
  expect_equal(nrow(result[result$site == "B", ]), 4)
})

test_that("even_timesteps errors on missing datetime column", {
  df <- data.frame(time = as.POSIXct("2024-01-01", tz = "UTC"))
  expect_snapshot(error = TRUE, even_timesteps(df))
})

test_that("even_timesteps errors on non-data.frame input", {
  expect_snapshot(error = TRUE, even_timesteps(1:10))
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
