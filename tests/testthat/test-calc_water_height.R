library(testthat)

test_that("calc_water_height behaves as expected under various conditions", {
  # Vented sensors
  sensor_kpa <- 19.2
  water_temp <- 15
  result_vented <- calc_water_height(
    sensor_kpa,
    water_temp = water_temp,
    type = "vented"
  )

  # Basic validations
  expect_true(is.numeric(result_vented))
  expect_gt(result_vented, 0) # Water height should be positive

  # Manual calculation for vented sensors
  gravity <- 9.80665
  water_density <- calc_water_density(water_temp)
  delta_Pa <- sensor_kpa * 1000
  expected_height_vented <- delta_Pa / (water_density * gravity)

  expect_equal(result_vented, expected_height_vented, tolerance = 1e-6)

  # Unvented sensors
  sensor_kpa <- 120.5
  atmo_kpa <- 101.3
  result_unvented <- calc_water_height(
    sensor_kpa,
    atmo_kpa = atmo_kpa,
    water_temp = water_temp,
    type = "unvented"
  )

  # Basic validations
  expect_true(is.numeric(result_unvented))
  expect_gt(result_unvented, 0) # Water height should be positive

  # Manual calculation for unvented sensors
  delta_Pa <- (sensor_kpa - atmo_kpa) * 1000
  expected_height_unvented <- delta_Pa / (water_density * gravity)

  expect_equal(result_unvented, expected_height_unvented, tolerance = 1e-6)

  # Temperature extremes
  temp_high <- 40
  temp_low <- 0

  result_high <- calc_water_height(
    sensor_kpa,
    water_temp = temp_high,
    type = "vented"
  )
  result_low <- calc_water_height(
    sensor_kpa,
    water_temp = temp_low,
    type = "vented"
  )

  expect_true(is.numeric(result_high) && is.numeric(result_low))
  expect_gt(result_high, 0)
  expect_gt(result_low, 0)

  # Zero pressure
  water_temp <- 25
  sensor_kpa <- 0
  result_zero <- calc_water_height(
    sensor_kpa,
    water_temp = water_temp,
    type = "vented"
  )

  expect_equal(result_zero, 0) # No pressure difference means zero water height

  # Vector inputs
  sensor_kpa <- c(19.2, 25.0)
  water_temp <- c(15, 20)
  result_vector <- calc_water_height(
    sensor_kpa,
    water_temp = water_temp,
    type = "vented"
  )

  expect_length(result_vector, length(sensor_kpa))
  expect_true(all(result_vector > 0))

  # Negative or zero atmospheric pressure
  water_temp <- 25
  sensor_kpa <- 120.5
  atmo_kpa <- 0
  result_atmo_zero <- calc_water_height(
    sensor_kpa,
    atmo_kpa = atmo_kpa,
    water_temp = water_temp,
    type = "unvented"
  )

  expect_gt(result_atmo_zero, 0)
})

test_that("calc_water_height errors when atmo_kpa is NULL for unvented type", {
  expect_snapshot(
    error = TRUE,
    calc_water_height(120.5, water_temp = 15, type = "unvented")
  )
})

test_that("calc_water_height(sensor_kPa) is deprecated", {
  expect_snapshot(
    invisible(calc_water_height(sensor_kPa = 19.2, water_temp = 15))
  )
})

test_that("calc_water_height(atmo_kPa) is deprecated", {
  expect_snapshot(
    invisible(calc_water_height(
      sensor_kpa = 120.5,
      atmo_kPa = 101.3,
      water_temp = 15,
      type = "unvented"
    ))
  )
})
