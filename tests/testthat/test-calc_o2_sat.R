calc_garcia_benson_mgL <- function(temp, salinity) {
  mgL_mlL <- 1.42905
  TS <- log((298.15 - temp) / (273.15 + temp))

  lnC <- 2.00907 +
    3.22014 * TS +
    4.05010 * TS^2 +
    4.94457 * TS^3 -
    2.56847e-1 * TS^4 +
    3.88767 * TS^5 -
    salinity *
      (6.24523e-3 +
        7.37614e-3 * TS +
        1.03410e-2 * TS^2 +
        8.17083e-3 * TS^3) -
    4.88682e-7 * salinity^2

  exp(lnC) * mgL_mlL
}

test_that("calc_o2_sat returns expected value at 25C, 1 atm, freshwater", {
  expect_equal(calc_o2_sat(25, 1, "atm", 0), 8.263376, tolerance = 1e-4)
})

test_that("calc_o2_sat O2 saturation decreases with temperature", {
  o2_cold <- calc_o2_sat(10, 1, "atm", 0)
  o2_warm <- calc_o2_sat(25, 1, "atm", 0)
  expect_gt(o2_cold, o2_warm)
})

test_that("calc_o2_sat O2 saturation decreases with salinity", {
  o2_fresh <- calc_o2_sat(25, 1, "atm", 0)
  o2_salt <- calc_o2_sat(25, 1, "atm", 35)
  expect_gt(o2_fresh, o2_salt)
})

test_that("calc_o2_sat accepts pressure in multiple units", {
  expect_equal(
    calc_o2_sat(25, 1, "atm", 0),
    calc_o2_sat(25, 101.325, "kPa", 0),
    tolerance = 1e-4
  )
  expect_equal(
    calc_o2_sat(25, 1, "atm", 0),
    calc_o2_sat(25, 1013.25, "mbar", 0),
    tolerance = 1e-4
  )
})

test_that("calc_o2_sat is vectorized over temperature", {
  result <- calc_o2_sat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(min(result), 0)
})

test_that("calc_o2_sat uses corrected vapor pressure at non-standard pressure", {
  standard <- calc_o2_sat(25, 1, "atm", 0)
  low_pressure <- calc_o2_sat(25, 900, "mbar", 0)
  vapor_press <- calc_vapor_press(25, salinity = 0, method = "Dickson2007")
  expected_corr <- (convert_pressure(900, "mbar", "atm") - vapor_press) /
    (1 - vapor_press)

  expect_equal(low_pressure, standard * expected_corr, tolerance = 1e-8)
})

test_that("calc_o2_sat returns umol/L consistent with mg/L", {
  mgL <- calc_o2_sat(25, 1, "atm", 0)
  umolL <- calc_o2_sat(25, 1, "atm", 0, out_units = "umol/L")
  expect_equal(umolL, 258.2305, tolerance = 1e-3)
  # mg/L = umol/L * 31.99880 g/mol / 1000
  expect_equal(mgL, umolL * 31.99880 / 1000, tolerance = 1e-8)
})

test_that("calc_o2_sat matches Garcia-Benson volume coefficients", {
  temp <- c(0, 10, 25, 30)
  salinity <- c(0, 0, 35, 35)

  expect_equal(
    calc_o2_sat(temp, 1013.25, "mbar", salinity),
    calc_garcia_benson_mgL(temp, salinity),
    tolerance = 0.002
  )
})

test_that("calc_O2sat() is deprecated", {
  expect_snapshot(invisible(calc_O2sat(25, 1, "atm", 0)))
})
