test_that("calc_ar_sat reproduces the Hamme and Emerson (2004) check value", {
  # Table 4 check value: 13.4622 umol/kg at T=10C, S=35
  density_kgL <- calc_water_density(10, 35) / 1000
  expect_equal(
    calc_ar_sat(10, 1, "atm", 35) / density_kgL,
    13.4622,
    tolerance = 1e-4
  )
})

test_that("calc_ar_sat returns expected values at 20C, 1 atm, freshwater", {
  expect_equal(calc_ar_sat(20, 1, "atm", 0), 13.92336, tolerance = 1e-5)
  expect_equal(
    calc_ar_sat(20, 1, "atm", 0, out_units = "mg/L"),
    0.5562105,
    tolerance = 1e-5
  )
})

test_that("calc_ar_sat mg/L is consistent with umol/L via molar mass", {
  umol <- calc_ar_sat(20, 1, "atm", 0)
  expect_equal(
    calc_ar_sat(20, 1, "atm", 0, out_units = "mg/L"),
    umol * 1e-6 * 39.948 * 1e3,
    tolerance = 1e-10
  )
})

test_that("calc_ar_sat decreases with temperature and salinity", {
  expect_gt(calc_ar_sat(10, 1, "atm", 0), calc_ar_sat(25, 1, "atm", 0))
  expect_gt(calc_ar_sat(20, 1, "atm", 0), calc_ar_sat(20, 1, "atm", 35))
})

test_that("calc_ar_sat is vectorized over temperature", {
  result <- calc_ar_sat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(result[1], result[3])
})

test_that("calc_ar_sat validates its inputs", {
  expect_snapshot(error = TRUE, calc_ar_sat("x", 1, "atm", 0))
  expect_snapshot(
    error = TRUE,
    calc_ar_sat(20, 1, "atm", 0, out_units = "mol/L")
  )
})

test_that("calc_Arsat() is deprecated", {
  expect_snapshot(invisible(calc_Arsat(20, 1, "atm", 0)))
})
