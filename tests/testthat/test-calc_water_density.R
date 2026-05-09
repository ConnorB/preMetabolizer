test_that("calc_water_density returns expected Kell 1975 values", {
  expect_equal(calc_water_density(0), 999.8395, tolerance = 1e-3)
  expect_equal(calc_water_density(25), 997.0449, tolerance = 1e-3)
  expect_equal(calc_water_density(100), 958.3637, tolerance = 1e-3)
})

test_that("calc_water_density peaks near 4C", {
  d3 <- calc_water_density(3)
  d4 <- calc_water_density(4)
  d5 <- calc_water_density(5)

  expect_gt(d4, d3)
  expect_gt(d4, d5)
})

test_that("calc_water_density is vectorized", {
  result <- calc_water_density(c(0, 10, 20, 30))

  expect_length(result, 4)
  expect_true(all(result > 900))
})

test_that("calc_water_density warns for temperatures outside valid range", {
  expect_snapshot(
    calc_water_density(c(-1, 20, 151))
  )
})

test_that("calc_water_density errors on non-numeric input", {
  expect_snapshot(
    error = TRUE,
    calc_water_density("warm")
  )
})
