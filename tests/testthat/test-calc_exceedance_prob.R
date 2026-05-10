test_that("calc_exceedance_prob gives correct Weibull positions", {
  result <- calc_exceedance_prob(c(10, 20, 30))
  # ranks: 30=1, 20=2, 10=3; n=3; Weibull = rank/(n+1)
  expect_equal(result, c(0.75, 0.5, 0.25))
})

test_that("calc_exceedance_prob output is between 0 and 1", {
  result <- calc_exceedance_prob(c(10, 5, 20, 15))
  expect_true(all(result > 0 & result < 1, na.rm = TRUE))
})

test_that("calc_exceedance_prob preserves NA positions", {
  result <- calc_exceedance_prob(c(10, NA, 30))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
})

test_that("calc_exceedance_prob rm.zero fills zero positions with NA", {
  result <- calc_exceedance_prob(c(10, 0, 30), rm.zero = TRUE)
  expect_true(is.na(result[2]))
})

test_that("calc_exceedance_prob errors on non-numeric input", {
  expect_snapshot(error = TRUE, calc_exceedance_prob("abc"))
})

test_that("calc_exceedance_prob errors on Inf values", {
  expect_snapshot(error = TRUE, calc_exceedance_prob(c(1, Inf, 3)))
})

test_that("rcpp_calc_exceedance_prob matches R implementation", {
  flow <- c(10, 5, 0, 15, 8, NA, 0, 20, 10)

  expect_equal(
    rcpp_calc_exceedance_prob(flow),
    calc_exceedance_prob(flow)
  )
  expect_equal(
    rcpp_calc_exceedance_prob(flow, rm.zero = TRUE),
    calc_exceedance_prob(flow, rm.zero = TRUE)
  )
})

test_that("rcpp_calc_exceedance_prob handles ties with average ranks", {
  result <- rcpp_calc_exceedance_prob(c(10, 20, 20, 30))

  expect_equal(result, c(0.8, 0.5, 0.5, 0.2))
})

test_that("rcpp_calc_exceedance_prob preserves NA positions", {
  result <- rcpp_calc_exceedance_prob(c(10, NA, 30))

  expect_equal(result[2], NA_real_)
  expect_equal(result[1], 2 / 3)
})
