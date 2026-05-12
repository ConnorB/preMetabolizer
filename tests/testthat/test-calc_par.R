test_that("calc_par returns 0 at solar midnight", {
  solar_midnight <- as.POSIXct("2024-06-21 00:01:00", tz = "UTC")
  par <- calc_par(solar_midnight, latitude = 39.1, longitude = -96.6)
  expect_equal(par, 0)
})

test_that("calc_par returns positive PAR near solar noon in summer", {
  noon <- convert_to_solar_time(
    as.POSIXct("2024-06-21 12:00:00", tz = "UTC"),
    longitude = -96.6
  )
  par <- calc_par(noon, latitude = 39.1, longitude = -96.6)
  expect_gt(par, 0)
})

test_that("calc_par is higher at solar noon than at dawn", {
  noon <- convert_to_solar_time(
    as.POSIXct("2024-06-21 12:00:00", tz = "UTC"),
    -96.6
  )
  dawn <- convert_to_solar_time(
    as.POSIXct("2024-06-21 06:00:00", tz = "UTC"),
    -96.6
  )
  expect_gt(calc_par(noon, 39.1, -96.6), calc_par(dawn, 39.1, -96.6))
})
