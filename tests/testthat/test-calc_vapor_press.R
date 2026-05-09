test_that("calc_vapor_press Dickson2007 returns expected value at 25C freshwater", {
  expect_equal(
    calc_vapor_press(25, 0, "Dickson2007"),
    0.03128374,
    tolerance = 1e-5
  )
})

test_that("calc_vapor_press MIMSY returns physically plausible value at 20C", {
  result <- calc_vapor_press(20, 0, "MIMSY")
  expect_equal(result, 0.02300875, tolerance = 1e-5)
})

test_that("calc_vapor_press increases with temperature", {
  vp_cold <- calc_vapor_press(10, 0, "Dickson2007")
  vp_warm <- calc_vapor_press(30, 0, "Dickson2007")
  expect_gt(vp_warm, vp_cold)
})

test_that("calc_vapor_press Dickson2007 decreases with salinity", {
  vp_fresh <- calc_vapor_press(25, 0, "Dickson2007")
  vp_salt <- calc_vapor_press(25, 35, "Dickson2007")
  expect_gt(vp_fresh, vp_salt)
})

test_that("calc_vapor_press MIMSY warns for non-zero salinity", {
  expect_snapshot(calc_vapor_press(20, 10, "MIMSY"))
})

test_that("calc_vapor_press errors on invalid method", {
  expect_snapshot(error = TRUE, calc_vapor_press(20, 0, "invalid"))
})
