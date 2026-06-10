test_that("weiss_price_F reproduces Weiss & Price (1980) gravimetric tables", {
  density_kgL <- calc_water_density(10, 34) / 1000

  # Table VII: CO2 gravimetric F = 4.342e-2 mol/(kg atm) at T=10C, S=34.
  F_co2 <- weiss_price_F(
    10,
    34,
    -160.7333,
    215.4152,
    89.8920,
    -1.47759,
    0.029941,
    -0.027455,
    0.0053407
  )
  expect_equal(F_co2 / density_kgL, 0.04342, tolerance = 0.01)

  # Table V: N2O gravimetric F = 3.186e-2 mol/(kg atm) at T=10C, S=34.
  F_n2o <- weiss_price_F(
    10,
    34,
    -165.8806,
    222.8743,
    92.0792,
    -1.48425,
    -0.056235,
    0.031619,
    -0.0048472
  )
  expect_equal(F_n2o / density_kgL, 0.03186, tolerance = 0.01)
})

test_that("calc_CO2sat returns expected values at 20C, 1 atm, freshwater", {
  expect_equal(calc_CO2sat(20, 1, "atm", 0), 15.99144, tolerance = 1e-4)
  expect_equal(
    calc_CO2sat(20, 1, "atm", 0, out_units = "mg/L"),
    0.7037753,
    tolerance = 1e-4
  )
})

test_that("calc_CO2sat equals x' * F at 1 atm", {
  F_co2 <- weiss_price_F(
    15,
    5,
    -160.7333,
    215.4152,
    89.8920,
    -1.47759,
    0.029941,
    -0.027455,
    0.0053407
  )
  expect_equal(
    calc_CO2sat(15, 1, "atm", 5, xCO2_ppm = 420, out_units = "umol/L"),
    420e-6 * F_co2 * 1e6,
    tolerance = 1e-10
  )
})

test_that("calc_CO2sat mg/L is consistent with umol/L via molar mass", {
  umol <- calc_CO2sat(20, 1, "atm", 0)
  expect_equal(
    calc_CO2sat(20, 1, "atm", 0, out_units = "mg/L"),
    umol * 1e-6 * 44.0095 * 1e3,
    tolerance = 1e-10
  )
})

test_that("calc_CO2sat decreases with temperature and salinity", {
  expect_gt(calc_CO2sat(10, 1, "atm", 0), calc_CO2sat(25, 1, "atm", 0))
  expect_gt(calc_CO2sat(20, 1, "atm", 0), calc_CO2sat(20, 1, "atm", 35))
})

test_that("calc_CO2sat increases with atmospheric mole fraction", {
  expect_gt(
    calc_CO2sat(20, 1, "atm", 0, xCO2_ppm = 800),
    calc_CO2sat(20, 1, "atm", 0, xCO2_ppm = 420)
  )
})

test_that("calc_CO2sat is vectorized over temperature", {
  result <- calc_CO2sat(c(10, 20, 25), 1, "atm", 0)
  expect_length(result, 3)
  expect_gt(result[1], result[3])
})

test_that("calc_CO2sat validates its inputs", {
  expect_snapshot(error = TRUE, calc_CO2sat("x", 1, "atm", 0))
  expect_snapshot(
    error = TRUE,
    calc_CO2sat(20, 1, "atm", 0, out_units = "mol/L")
  )
})
