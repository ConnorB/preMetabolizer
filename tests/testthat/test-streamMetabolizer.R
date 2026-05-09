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
