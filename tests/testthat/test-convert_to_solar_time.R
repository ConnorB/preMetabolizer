test_that("convert_to_solar_time shifts time eastward for positive longitude", {
  t_utc <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  t_east <- convert_to_solar_time(t_utc, 15)
  t_west <- convert_to_solar_time(t_utc, -15)
  expect_gt(as.numeric(t_east), as.numeric(t_west))
})

test_that("convert_to_solar_time mean offset matches 15 deg per hour", {
  t_utc <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  t_solar <- convert_to_solar_time(t_utc, -90)
  expect_equal(
    as.numeric(t_solar) - as.numeric(t_utc),
    -90 / 15 * 3600
  )
})

test_that("convert_to_solar_time returns solar_date class for mean", {
  t_utc <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  t_solar <- convert_to_solar_time(t_utc, -90)
  expect_s3_class(t_solar, "solar_date")
  expect_s3_class(t_solar, "POSIXct")
  expect_identical(attr(t_solar, "tzone"), "UTC")
})

test_that("convert_to_solar_time apparent differs from mean by equation of time", {
  t_utc <- as.POSIXct("2024-02-11 12:00:00", tz = "UTC")
  mean_t <- convert_to_solar_time(t_utc, -90, type = "mean")
  app_t <- convert_to_solar_time(t_utc, -90, type = "apparent")
  # Equation of time is around -14 minutes in mid-February.
  expect_gt(abs(as.numeric(app_t) - as.numeric(mean_t)), 60)
})

test_that("convert_to_solar_time accepts character input", {
  t_solar <- convert_to_solar_time("2024-06-21 12:00:00", -90)
  expect_s3_class(t_solar, "POSIXct")
  expect_equal(
    format(t_solar, "%Y-%m-%d %H:%M:%S"),
    "2024-06-21 06:00:00"
  )
})

test_that("convert_to_solar_time converts non-UTC tz to UTC before offsetting", {
  t_local <- as.POSIXct("2024-06-21 07:00:00", tz = "America/Chicago")
  t_solar <- convert_to_solar_time(t_local, -90)
  # 2024-06-21 07:00 America/Chicago = 12:00 UTC -> 06:00 mean solar at -90.
  expect_equal(
    format(t_solar, "%Y-%m-%d %H:%M:%S"),
    "2024-06-21 06:00:00"
  )
})

test_that("convert_from_solar_time inverts convert_to_solar_time for mean", {
  t_utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
  t_solar <- convert_to_solar_time(t_utc, -96.6)
  t_back <- convert_from_solar_time(t_solar, -96.6)
  expect_equal(as.numeric(t_back), as.numeric(t_utc))
})

test_that("convert_from_solar_time inverts convert_to_solar_time for apparent within tolerance", {
  t_utc <- as.POSIXct("2024-02-11 12:00:00", tz = "UTC")
  t_solar <- convert_to_solar_time(t_utc, -96.6, type = "apparent")
  t_back <- convert_from_solar_time(t_solar, -96.6, type = "apparent")
  # Inverse uses EoT at solar instant rather than UTC; daily EoT change
  # is ~30 s, so 60 s tolerance is generous.
  expect_lt(abs(as.numeric(t_back) - as.numeric(t_utc)), 60)
})

test_that("convert_from_solar_time returns POSIXct without solar_date class", {
  t_utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
  t_back <- convert_from_solar_time(convert_to_solar_time(t_utc, -90), -90)
  expect_s3_class(t_back, "POSIXct")
  expect_false(inherits(t_back, "solar_date"))
})

test_that("convert_to_solar_time is vectorised", {
  t_utc <- as.POSIXct(c("2024-06-21 00:00", "2024-06-21 12:00"), tz = "UTC")
  t_solar <- convert_to_solar_time(t_utc, -90)
  expect_length(t_solar, 2)
  expect_equal(diff(as.numeric(t_solar)), 12 * 3600)
})
