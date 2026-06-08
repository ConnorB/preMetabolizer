test_that("calc_CH4sat matches Wiesenburg & Guinasso (1979) Table VI", {
  # Table VI test value: 2.146 nmol/L at T=10C, S=34.8, x'=1.41e-6, 1 atm.
  expect_equal(
    calc_CH4sat(10, 1, "atm", 34.8, xCH4_ppm = 1.41, out_units = "umol/L") *
      1000,
    2.146,
    tolerance = 0.01
  )
})

test_that("calc_CH4sat returns expected value at 20C, 1 atm, freshwater", {
  expect_equal(calc_CH4sat(20, 1, "atm", 0), 0.002873362, tolerance = 1e-5)
})

test_that("calc_CH4sat mg/L is consistent with umol/L via molar mass", {
  umol <- calc_CH4sat(20, 1, "atm", 0, out_units = "umol/L")
  expect_equal(
    calc_CH4sat(20, 1, "atm", 0, out_units = "mg/L"),
    umol * 16.0425 / 1000,
    tolerance = 1e-10
  )
})

test_that("calc_CH4sat decreases with temperature and salinity", {
  expect_gt(calc_CH4sat(10, 1, "atm", 0), calc_CH4sat(25, 1, "atm", 0))
  expect_gt(calc_CH4sat(20, 1, "atm", 0), calc_CH4sat(20, 1, "atm", 35))
})

test_that("calc_CH4sat increases with atmospheric mole fraction", {
  expect_gt(
    calc_CH4sat(20, 1, "atm", 0, xCH4_ppm = 3),
    calc_CH4sat(20, 1, "atm", 0, xCH4_ppm = 1.9)
  )
})

test_that("calc_CH4sat accepts pressure in multiple units", {
  expect_equal(
    calc_CH4sat(20, 1, "atm", 0),
    calc_CH4sat(20, 101.325, "kPa", 0),
    tolerance = 1e-8
  )
})

test_that("calc_CH4sat is vectorized over temperature", {
  result <- calc_CH4sat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(result[1], result[3])
})
