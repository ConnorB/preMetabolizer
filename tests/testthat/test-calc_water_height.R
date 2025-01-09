library(testthat)

test_that("calc_water_height behaves as expected under various conditions", {
  # Vented sensors
  sensor_kPa <- 19.2
  tempC <- 15
  result_vented <- calc_water_height(sensor_kPa, tempC = tempC, type = "vented")

  # Basic validations
  expect_true(is.numeric(result_vented))
  expect_gt(result_vented, 0) # Water height should be positive

  # Manual calculation for vented sensors
  gravity <- 9.80665
  water_density <- (999.83952 + 16.945176 * tempC + (-7.9870401e-3) * tempC^2 +
                      (-46.170461e-6) * tempC^3 + 105.56302e-9 * tempC^4 +
                      (-280.54253e-12) * tempC^5) / (1 + 16.87985e-3 * tempC)
  delta_Pa <- sensor_kPa * 1000
  expected_height_vented <- delta_Pa / (water_density * gravity)

  expect_equal(result_vented, expected_height_vented, tolerance = 1e-6)

  # Unvented sensors
  sensor_kPa <- 120.5
  atmo_kPa <- 101.3
  result_unvented <- calc_water_height(sensor_kPa, atmo_kPa = atmo_kPa, tempC = tempC, type = "unvented")

  # Basic validations
  expect_true(is.numeric(result_unvented))
  expect_gt(result_unvented, 0) # Water height should be positive

  # Manual calculation for unvented sensors
  delta_Pa <- (sensor_kPa - atmo_kPa) * 1000
  expected_height_unvented <- delta_Pa / (water_density * gravity)

  expect_equal(result_unvented, expected_height_unvented, tolerance = 1e-6)

  # Temperature extremes
  temp_high <- 40
  temp_low <- 0

  result_high <- calc_water_height(sensor_kPa, tempC = temp_high, type = "vented")
  result_low <- calc_water_height(sensor_kPa, tempC = temp_low, type = "vented")

  expect_true(is.numeric(result_high) && is.numeric(result_low))
  expect_gt(result_high, 0)
  expect_gt(result_low, 0)

  # Zero pressure
  tempC <- 25
  sensor_kPa <- 0
  result_zero <- calc_water_height(sensor_kPa, tempC = tempC, type = "vented")

  expect_equal(result_zero, 0) # No pressure difference means zero water height

  # Vector inputs
  sensor_kPa <- c(19.2, 25.0)
  tempC <- c(15, 20)
  result_vector <- calc_water_height(sensor_kPa, tempC = tempC, type = "vented")

  expect_length(result_vector, length(sensor_kPa))
  expect_true(all(result_vector > 0))

  # Negative or zero atmospheric pressure
  tempC <- 25
  sensor_kPa <- 120.5
  atmo_kPa <- 0
  result_atmo_zero <- calc_water_height(sensor_kPa, atmo_kPa = atmo_kPa, tempC = tempC, type = "unvented")

  expect_gt(result_atmo_zero, 0)

})
