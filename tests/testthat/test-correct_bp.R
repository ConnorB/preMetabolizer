library(testthat)

test_that("correct_bp works correctly for positive elevation difference", {
  station_kPa <- c(101.3, 100.5)
  air_temp <- c(15, 10)
  station_elev <- 300
  site_elev <- 500
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Expected output for each set of inputs
  expect_length(result, 2)
  expect_true(all(result < station_kPa)) # Pressure should decrease at higher elevation
})

test_that("correct_bp works correctly for negative elevation difference", {
  station_kPa <- c(101.3, 100.5)
  air_temp <- c(15, 10)
  station_elev <- 500
  site_elev <- 300
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Expected output for each set of inputs
  expect_length(result, 2)
  expect_true(all(result > station_kPa)) # Pressure should increase at lower elevation
})

test_that("correct_bp handles zero elevation difference", {
  station_kPa <- c(101.3, 100.5)
  air_temp <- c(15, 10)
  station_elev <- 300
  site_elev <- 300
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Output should equal input pressures
  expect_equal(result, station_kPa)
})

test_that("correct_bp handles vector inputs", {
  station_kPa <- c(101.3, 100.5, 99.8)
  air_temp <- c(15, 10, 5)
  station_elev <- 300
  site_elev <- 500
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Check length matches inputs
  expect_length(result, length(station_kPa))
})

test_that("correct_bp returns NA for invalid inputs", {
  station_kPa <- c(101.3, NA)
  air_temp <- c(15, 10)
  station_elev <- 300
  site_elev <- 500
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Expect NA in the output where input is NA
  expect_true(is.na(result[2]))
})

test_that("correct_bp handles edge cases", {
  # Extreme values
  station_kPa <- c(50, 150)
  air_temp <- c(-50, 50)
  station_elev <- 0
  site_elev <- 1000
  result <- correct_bp(station_kPa, air_temp, station_elev, site_elev)

  # Ensure the function runs without errors and outputs a result
  expect_length(result, length(station_kPa))
  expect_true(all(result > 0)) # Pressure should be positive
})
