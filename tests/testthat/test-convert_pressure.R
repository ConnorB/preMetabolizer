test_that("convert_pressure kPa to atm: 101.325 kPa = 1 atm", {
  expect_equal(convert_pressure(101.325, "kPa", "atm"), 1, tolerance = 1e-6)
})

test_that("convert_pressure 1 atm to Pa = 101325", {
  expect_equal(convert_pressure(1, "atm", "Pa"), 101325)
})

test_that("convert_pressure is case-insensitive", {
  expect_equal(
    convert_pressure(101.325, "kPa", "atm"),
    convert_pressure(101.325, "KPA", "ATM")
  )
})

test_that("convert_pressure same-unit conversion is identity", {
  expect_equal(convert_pressure(100, "hPa", "hPa"), 100)
})

test_that("convert_pressure is vectorized", {
  result <- convert_pressure(c(101.325, 100, 90), "kPa", "atm")
  expect_length(result, 3)
})

test_that("convert_pressure converts common units to atm", {
  expect_equal(convert_pressure(1, "atm", "atm"), 1)
  expect_equal(convert_pressure(1013.25, "hPa", "atm"), 1, tolerance = 1e-6)
  expect_equal(convert_pressure(101.325, "kPa", "atm"), 1, tolerance = 1e-6)
})

test_that("correct_bp gives lower pressure at higher elevation", {
  bp_low <- correct_bp(101.3, 15, 300, 300) # same elevation
  bp_high <- correct_bp(101.3, 15, 300, 800) # higher site
  expect_lt(bp_high, bp_low)
})

test_that("correct_bp same elevation returns same pressure", {
  result <- correct_bp(101.3, 15, 300, 300)
  expect_equal(result, 101.3, tolerance = 1e-6)
})

test_that("correct_bp returns expected value", {
  expect_equal(correct_bp(101.3, 15, 300, 500), 98.92626, tolerance = 1e-3)
})

test_that("correct_bp honors from_units and to_units", {
  kpa_result <- correct_bp(101.3, 15, 300, 500)
  hpa_input <- correct_bp(
    1013,
    15,
    300,
    500,
    from_units = "hPa",
    to_units = "kPa"
  )
  expect_equal(hpa_input, kpa_result, tolerance = 1e-3)

  hpa_output <- correct_bp(
    101.3,
    15,
    300,
    500,
    from_units = "kPa",
    to_units = "hPa"
  )
  expect_equal(hpa_output, kpa_result * 10, tolerance = 1e-3)
})

test_that("correct_bp is vectorized over station_bp and air_temp", {
  result <- correct_bp(c(101.3, 100.5), c(15, 10), 300, 500)
  expect_length(result, 2)
})
