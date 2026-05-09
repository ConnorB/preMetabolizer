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

test_that("convert_pressure_to_atm converts common units correctly", {
  expect_equal(convert_pressure_to_atm(1, "atm"), 1)
  expect_equal(convert_pressure_to_atm(1013.25, "hPa"), 1, tolerance = 1e-6)
  expect_equal(convert_pressure_to_atm(101.325, "kPa"), 1, tolerance = 1e-6)
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
