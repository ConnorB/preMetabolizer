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
