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

test_that("calc_par models multiple sites in one call", {
  noon <- convert_to_solar_time(
    as.POSIXct(c("2024-06-21 12:00:00", "2024-06-21 12:00:00"), tz = "UTC"),
    longitude = c(-96.6, -120)
  )
  multi <- calc_par(
    noon,
    latitude = c(39.1, 47.6),
    longitude = c(-96.6, -120)
  )
  single1 <- calc_par(noon[1], latitude = 39.1, longitude = -96.6)
  single2 <- calc_par(noon[2], latitude = 47.6, longitude = -120)
  expect_equal(multi, c(single1, single2))
})

test_that("calc_par errors on a coordinate length that is neither 1 nor n", {
  noon <- convert_to_solar_time(
    as.POSIXct(c("2024-06-21 12:00:00", "2024-06-21 12:00:00"), tz = "UTC"),
    longitude = -96.6
  )
  expect_snapshot(
    calc_par(noon, latitude = c(39.1, 40, 41), longitude = -96.6),
    error = TRUE
  )
})
