test_that("convert_UTC_to_solartime shifts time by longitude", {
  t_utc <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  t_solar_east <- convert_UTC_to_solartime(t_utc, 15, "mean solar")
  t_solar_west <- convert_UTC_to_solartime(t_utc, -15, "mean solar")
  expect_gt(as.numeric(t_solar_east), as.numeric(t_solar_west))
})

test_that("convert_UTC_to_solartime and convert_solartime_to_UTC are inverse", {
  t_utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
  t_solar <- convert_UTC_to_solartime(t_utc, -96.6, "mean solar")
  t_back <- convert_solartime_to_UTC(t_solar, -96.6, "mean solar")
  expect_equal(as.numeric(t_back), as.numeric(t_utc))
})

test_that("convert_UTC_to_solartime mean offset matches 3.989 min per degree", {
  t_utc <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  t_solar <- convert_UTC_to_solartime(t_utc, -90, "mean solar")
  expect_equal(
    as.numeric(t_solar) - as.numeric(t_utc),
    -90 * 3.989 * 60
  )
})

test_that("convert_UTC_to_solartime apparent solar differs from mean solar", {
  t_utc <- as.POSIXct("2024-02-11 12:00:00", tz = "UTC")
  mean_t <- convert_UTC_to_solartime(t_utc, -90, "mean solar")
  app_t <- convert_UTC_to_solartime(t_utc, -90, "apparent solar")
  # Equation of time is non-zero in February.
  expect_gt(abs(as.numeric(app_t) - as.numeric(mean_t)), 60)
})

test_that("convert_UTC_to_solartime and convert_solartime_to_UTC roundtrip apparent solar", {
  t_utc <- as.POSIXct("2024-02-11 12:00:00", tz = "UTC")
  t_solar <- convert_UTC_to_solartime(t_utc, -96.6, "apparent solar")
  t_back <- convert_solartime_to_UTC(t_solar, -96.6, "apparent solar")
  expect_equal(as.numeric(t_back), as.numeric(t_utc), tolerance = 1e-6)
})

test_that("convert_UTC_to_solartime errors on non-POSIXct input", {
  expect_snapshot(error = TRUE, convert_UTC_to_solartime("2024-06-21", -90))
})

test_that("convert_UTC_to_solartime errors on non-UTC timezone", {
  t_local <- as.POSIXct("2024-06-21 12:00:00", tz = "America/Chicago")
  expect_snapshot(error = TRUE, convert_UTC_to_solartime(t_local, -90))
})

test_that("convert_solartime_to_UTC errors on non-POSIXct input", {
  expect_snapshot(error = TRUE, convert_solartime_to_UTC("2024-06-21", -90))
})

test_that("convert_solartime_to_UTC errors on non-UTC timezone", {
  t_local <- as.POSIXct("2024-06-21 12:00:00", tz = "America/Chicago")
  expect_snapshot(error = TRUE, convert_solartime_to_UTC(t_local, -90))
})

test_that("calc_light returns 0 at night (solar midnight)", {
  solar_midnight <- as.POSIXct("2024-06-21 00:01:00", tz = "UTC")
  par <- calc_light(solar_midnight, latitude = 39.1, longitude = -96.6)
  expect_equal(par, 0)
})

test_that("calc_light returns positive PAR near solar noon in summer", {
  noon <- convert_UTC_to_solartime(
    as.POSIXct("2024-06-21 12:00:00", tz = "UTC"),
    longitude = -96.6,
    time.type = "mean solar"
  )
  par <- calc_light(noon, latitude = 39.1, longitude = -96.6)
  expect_gt(par, 0)
})

test_that("calc_light is higher at solar noon than at dawn", {
  noon <- convert_UTC_to_solartime(
    as.POSIXct("2024-06-21 12:00:00", tz = "UTC"),
    -96.6,
    "mean solar"
  )
  dawn <- convert_UTC_to_solartime(
    as.POSIXct("2024-06-21 06:00:00", tz = "UTC"),
    -96.6,
    "mean solar"
  )
  expect_gt(calc_light(noon, 39.1, -96.6), calc_light(dawn, 39.1, -96.6))
})
