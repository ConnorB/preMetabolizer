test_that("calc_water_density returns expected Tanaka et al. (2001) values", {
  # Table 1 check values (de-aerated SMOW, ITS-90)
  expect_equal(calc_water_density(0), 999.8428, tolerance = 1e-5)
  expect_equal(calc_water_density(25), 997.0470, tolerance = 1e-5)
  expect_equal(calc_water_density(40), 992.2152, tolerance = 1e-5)
})

test_that("calc_water_density uses Kell (1975) above 40°C without warning", {
  # Kell (1975) Table III check values, beyond the Tanaka range
  expect_equal(calc_water_density(100), 958.3637, tolerance = 1e-4)
  expect_equal(calc_water_density(150), 916.829, tolerance = 1e-4)
  expect_no_warning(calc_water_density(50))
})

test_that("calc_water_density matches the Millero and Poisson (1981) check value", {
  # Table 1 check value: S = 35, t = 5C, 1 atm -> 1027.67547 kg/m^3
  expect_equal(
    calc_water_density(5, salinity = 35),
    1027.67547,
    tolerance = 1e-6
  )
})

test_that("calc_water_density defaults to freshwater", {
  expect_equal(calc_water_density(15), calc_water_density(15, salinity = 0))
})

test_that("calc_water_density increases with salinity", {
  expect_gt(calc_water_density(20, salinity = 35), calc_water_density(20))
})

test_that("calc_water_density is vectorized over salinity", {
  result <- calc_water_density(20, c(0, 35))
  expect_length(result, 2)
  expect_gt(result[2], result[1])
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
    calc_water_density(c(-1, 100, 200))
  )
})

test_that("calc_water_density warns outside the Millero and Poisson range", {
  expect_snapshot(
    calc_water_density(45, salinity = 35)
  )
  expect_snapshot(
    calc_water_density(20, salinity = 50)
  )
})

test_that("calc_water_density errors on non-numeric input", {
  expect_snapshot(
    error = TRUE,
    calc_water_density("warm")
  )
  expect_snapshot(
    error = TRUE,
    calc_water_density(15, salinity = "salty")
  )
})
