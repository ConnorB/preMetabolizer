test_that("calc_N2sat reproduces the Hamme and Emerson (2004) check value", {
  # Table 4 check value: 500.885 umol/kg at T=10C, S=35
  density_kgL <- calc_water_density(10, 35) / 1000
  expect_equal(
    calc_N2sat(10, 1, "atm", 35) / density_kgL,
    500.885,
    tolerance = 1e-6
  )
})

test_that("calc_N2sat returns expected values at 20C, 1 atm, freshwater", {
  expect_equal(calc_N2sat(20, 1, "atm", 0), 536.4781, tolerance = 1e-5)
  expect_equal(
    calc_N2sat(20, 1, "atm", 0, out_units = "mg/L"),
    15.02858,
    tolerance = 1e-5
  )
})

test_that("calc_N2sat mg/L is consistent with umol/L via molar mass", {
  umol <- calc_N2sat(20, 1, "atm", 0)
  expect_equal(
    calc_N2sat(20, 1, "atm", 0, out_units = "mg/L"),
    umol * 1e-6 * 28.0134 * 1e3,
    tolerance = 1e-10
  )
})

test_that("calc_N2sat decreases with temperature and salinity", {
  expect_gt(calc_N2sat(10, 1, "atm", 0), calc_N2sat(25, 1, "atm", 0))
  expect_gt(calc_N2sat(20, 1, "atm", 0), calc_N2sat(20, 1, "atm", 35))
})

test_that("calc_N2sat is vectorized over temperature", {
  result <- calc_N2sat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(result[1], result[3])
})

test_that("calc_N2sat validates its inputs", {
  expect_snapshot(error = TRUE, calc_N2sat("x", 1, "atm", 0))
  expect_snapshot(
    error = TRUE,
    calc_N2sat(20, 1, "atm", 0, out_units = "mol/L")
  )
})
