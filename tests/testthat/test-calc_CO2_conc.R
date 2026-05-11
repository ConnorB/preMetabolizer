test_that("calc_K0 decreases with increasing temperature", {
  k0_cold <- calc_K0(10, 0, 1, 0)
  k0_warm <- calc_K0(30, 0, 1, 0)
  expect_gt(k0_cold, k0_warm)
})

test_that("calc_K0 decreases with increasing salinity", {
  k0_fresh <- calc_K0(20, 0, 1, 0)
  k0_salt <- calc_K0(20, 0, 1, 35)
  expect_gt(k0_fresh, k0_salt)
})

test_that("calc_K0 is always positive", {
  expect_gt(calc_K0(0, 0, 1, 0), 0)
  expect_gt(calc_K0(40, 0, 1, 35), 0)
})

test_that("calc_K0 returns known value at 20C, freshwater, 1 atm", {
  expect_equal(calc_K0(20, 0, 1, 0), 0.03916223, tolerance = 1e-5)
})

test_that("calc_K0 is vectorized over temp_water", {
  result <- calc_K0(c(10, 20, 30))
  expect_length(result, 3)
  expect_gt(result[1], result[2])
  expect_gt(result[2], result[3])
})

test_that("calc_K0 pressure correction adjusts K0 with depth", {
  # Weiss & Price (1980) pressure correction: increased total pressure
  # decreases K0 (mol/kg/atm) via the partial molal volume term.
  k0_surface <- calc_K0(20, waterDepth_m = 0, atmo_press = 1, salinity = 0)
  k0_deep <- calc_K0(20, waterDepth_m = 10, atmo_press = 1, salinity = 0)
  expect_lt(k0_deep, k0_surface)
})

test_that("calc_CO2_molKg returns positive value for realistic inputs", {
  result <- calc_CO2_molKg(400, 20, 0, 101.325, "kPa", 0)
  expect_gt(result, 0)
  expect_equal(result, 1.530325e-05, tolerance = 1e-3)
})

test_that("calc_CO2_mgL returns positive value consistent with molKg", {
  result <- calc_CO2_mgL(400, 20, 0, 101.325, "kPa", 0)
  expect_gt(result, 0)
  expect_equal(result, 0.6722788, tolerance = 1e-4)
})

test_that("calc_CO2_mgL increases with higher CO2_ppm", {
  low <- calc_CO2_mgL(200, 20, 0, 101.325, "kPa", 0)
  high <- calc_CO2_mgL(800, 20, 0, 101.325, "kPa", 0)
  expect_gt(high, low)
})

test_that("calc_CO2_molKg follows Henry's law at non-standard atmo pressure", {
  # Regression: a prior implementation included a spurious factor of total
  # pressure that under-reported [CO2*] at high-elevation sites. The result
  # should equal K0 * (P_atm - P_vap) * xCO2 * 1e-6 exactly.
  xCO2 <- 400
  T <- 20
  S <- 0
  atmo <- 0.85 # atm — ~1500 m elevation
  K0 <- calc_K0(T, 0, atmo, S)
  vp <- calc_vapor_press(T, S)
  expected <- K0 * (atmo - vp) * xCO2 * 1e-6
  expect_equal(
    calc_CO2_molKg(xCO2, T, 0, atmo, "atm", S),
    expected,
    tolerance = 1e-10
  )
})

test_that("calc_CO2_mgL equals molKg * molar_mass_CO2 * density", {
  xCO2 <- 400
  T <- 20
  S <- 0
  atmo <- 101.325
  molKg <- calc_CO2_molKg(xCO2, T, 0, atmo, "kPa", S)
  density_kgL <- calc_water_density(T) / 1e3
  expect_equal(
    calc_CO2_mgL(xCO2, T, 0, atmo, "kPa", S),
    molKg * 44.0095 * 1e3 * density_kgL,
    tolerance = 1e-10
  )
})

test_that("calc_CO2_molKg pressure correction is via K0 only (no extra factor)", {
  # At depth, the only pressure effect should be the Weiss & Price (1980)
  # pressure correction inside calc_K0, not a separate multiplicative factor.
  xCO2 <- 400
  T <- 20
  S <- 0
  K0_surface <- calc_K0(T, 0, 1, S)
  K0_depth <- calc_K0(T, 10, 1, S)
  ratio_code <- calc_CO2_molKg(xCO2, T, 10, 1, "atm", S) /
    calc_CO2_molKg(xCO2, T, 0, 1, "atm", S)
  expect_equal(ratio_code, K0_depth / K0_surface, tolerance = 1e-10)
})
