test_that("calc_N2Osat returns expected values at 20C, 1 atm, freshwater", {
  expect_equal(calc_N2Osat(20, 1, "atm", 0), 0.009460648, tolerance = 1e-5)
  expect_equal(
    calc_N2Osat(20, 1, "atm", 0, out_units = "mg/L"),
    0.0004163896,
    tolerance = 1e-5
  )
})

test_that("calc_N2Osat equals x' * F at 1 atm", {
  F_n2o <- weiss_price_F(
    15,
    5,
    -165.8806,
    222.8743,
    92.0792,
    -1.48425,
    -0.056235,
    0.031619,
    -0.0048472
  )
  expect_equal(
    calc_N2Osat(15, 1, "atm", 5, xN2O_ppm = 0.338, out_units = "umol/L"),
    0.338e-6 * F_n2o * 1e6,
    tolerance = 1e-10
  )
})

test_that("calc_N2Osat mg/L is consistent with umol/L via molar mass", {
  umol <- calc_N2Osat(20, 1, "atm", 0)
  expect_equal(
    calc_N2Osat(20, 1, "atm", 0, out_units = "mg/L"),
    umol * 1e-6 * 44.0128 * 1e3,
    tolerance = 1e-10
  )
})

test_that("calc_N2Osat decreases with temperature and salinity", {
  expect_gt(calc_N2Osat(10, 1, "atm", 0), calc_N2Osat(25, 1, "atm", 0))
  expect_gt(calc_N2Osat(20, 1, "atm", 0), calc_N2Osat(20, 1, "atm", 35))
})

test_that("calc_N2Osat increases with atmospheric mole fraction", {
  expect_gt(
    calc_N2Osat(20, 1, "atm", 0, xN2O_ppm = 0.5),
    calc_N2Osat(20, 1, "atm", 0, xN2O_ppm = 0.338)
  )
})

test_that("calc_N2Osat is vectorized over temperature", {
  result <- calc_N2Osat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(result[1], result[3])
})
