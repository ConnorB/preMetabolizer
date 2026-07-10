test_that("calc_k0 decreases with increasing temperature", {
  k0_cold <- calc_k0(10, 0, 1, 0)
  k0_warm <- calc_k0(30, 0, 1, 0)
  expect_gt(k0_cold, k0_warm)
})

test_that("calc_k0 decreases with increasing salinity", {
  k0_fresh <- calc_k0(20, 0, 1, 0)
  k0_salt <- calc_k0(20, 0, 1, 35)
  expect_gt(k0_fresh, k0_salt)
})

test_that("calc_k0 is always positive", {
  expect_gt(calc_k0(0, 0, 1, 0), 0)
  expect_gt(calc_k0(40, 0, 1, 35), 0)
})

test_that("calc_k0 returns known value at 20C, freshwater, 1 atm", {
  expect_equal(calc_k0(20, 0, 1, 0), 0.03916223, tolerance = 1e-5)
})

test_that("calc_k0 is vectorized over temp_water", {
  result <- calc_k0(c(10, 20, 30))
  expect_length(result, 3)
  expect_gt(result[1], result[2])
  expect_gt(result[2], result[3])
})

test_that("calc_k0 pressure correction adjusts K0 with depth", {
  # Weiss & Price (1980) pressure correction: increased total pressure
  # decreases K0 (mol/kg/atm) via the partial molal volume term.
  k0_surface <- calc_k0(20, water_depth_m = 0, atmo_press = 1, salinity = 0)
  k0_deep <- calc_k0(20, water_depth_m = 10, atmo_press = 1, salinity = 0)
  expect_lt(k0_deep, k0_surface)
})

test_that("calc_co2_mol_kg returns positive value for realistic inputs", {
  result <- calc_co2_mol_kg(400, 20, 0, 101.325, "kPa", 0)
  expect_gt(result, 0)
  expect_equal(result, 1.530325e-05, tolerance = 1e-3)
})

test_that("calc_co2_mg_l returns positive value consistent with mol_kg", {
  result <- calc_co2_mg_l(400, 20, 0, 101.325, "kPa", 0)
  expect_gt(result, 0)
  expect_equal(result, 0.6722788, tolerance = 1e-4)
})

test_that("calc_co2_mg_l increases with higher co2_ppm", {
  low <- calc_co2_mg_l(200, 20, 0, 101.325, "kPa", 0)
  high <- calc_co2_mg_l(800, 20, 0, 101.325, "kPa", 0)
  expect_gt(high, low)
})

test_that("calc_co2_mol_kg follows Henry's law at non-standard atmo pressure", {
  # Regression: a prior implementation included a spurious factor of total
  # pressure that under-reported [CO2*] at high-elevation sites. The result
  # should equal K0 * (P_atm - P_vap) * xCO2 * 1e-6 exactly.
  xCO2 <- 400
  T <- 20
  S <- 0
  atmo <- 0.85 # atm — ~1500 m elevation
  K0 <- calc_k0(T, 0, atmo, S)
  vp <- calc_vapor_press(T, S)
  expected <- K0 * (atmo - vp) * xCO2 * 1e-6
  expect_equal(
    calc_co2_mol_kg(xCO2, T, 0, atmo, "atm", S),
    expected,
    tolerance = 1e-10
  )
})

test_that("calc_co2_mg_l equals mol_kg * molar_mass_CO2 * density", {
  xCO2 <- 400
  T <- 20
  S <- 0
  atmo <- 101.325
  molKg <- calc_co2_mol_kg(xCO2, T, 0, atmo, "kPa", S)
  density_kgL <- calc_water_density(T) / 1e3
  expect_equal(
    calc_co2_mg_l(xCO2, T, 0, atmo, "kPa", S),
    molKg * 44.0095 * 1e3 * density_kgL,
    tolerance = 1e-10
  )
})

test_that("calc_co2_mol_kg pressure correction is via K0 only (no extra factor)", {
  # At depth, the only pressure effect should be the Weiss & Price (1980)
  # pressure correction inside calc_k0, not a separate multiplicative factor.
  xCO2 <- 400
  T <- 20
  S <- 0
  K0_surface <- calc_k0(T, 0, 1, S)
  K0_depth <- calc_k0(T, 10, 1, S)
  ratio_code <- calc_co2_mol_kg(xCO2, T, 10, 1, "atm", S) /
    calc_co2_mol_kg(xCO2, T, 0, 1, "atm", S)
  expect_equal(ratio_code, K0_depth / K0_surface, tolerance = 1e-10)
})

test_that("calc_K0() is deprecated", {
  expect_snapshot(invisible(calc_K0(20)))
})

test_that("calc_K0(waterDepth_m) is deprecated", {
  expect_snapshot(invisible(calc_K0(20, waterDepth_m = 0.5)))
})

test_that("calc_CO2_molKg() is deprecated", {
  expect_snapshot(
    invisible(calc_CO2_molKg(
      CO2_ppm = 420,
      temp_water = 20,
      water_depth_m = 0,
      atmo_press = 101.325,
      press_units = "kPa"
    ))
  )
})

test_that("calc_CO2_molKg(waterDepth_m) is deprecated", {
  expect_snapshot(
    invisible(calc_CO2_molKg(
      CO2_ppm = 420,
      temp_water = 20,
      atmo_press = 101.325,
      press_units = "kPa",
      waterDepth_m = 0.5
    ))
  )
})

test_that("calc_CO2_mgL() is deprecated", {
  expect_snapshot(
    invisible(calc_CO2_mgL(
      CO2_ppm = 420,
      temp_water = 20,
      water_depth_m = 0,
      atmo_press = 101.325,
      press_units = "kPa"
    ))
  )
})

test_that("calc_CO2_mgL(waterDepth_m) is deprecated", {
  expect_snapshot(
    invisible(calc_CO2_mgL(
      CO2_ppm = 420,
      temp_water = 20,
      atmo_press = 101.325,
      press_units = "kPa",
      waterDepth_m = 0.5
    ))
  )
})
