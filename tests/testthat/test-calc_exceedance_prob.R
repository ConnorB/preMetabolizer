test_that("calc_exceedance_prob gives correct Weibull positions", {
  result <- calc_exceedance_prob(c(10, 20, 30))
  # ranks: 30=1, 20=2, 10=3; n=3; Weibull = rank/(n+1)
  expect_equal(result, c(0.75, 0.5, 0.25))
})

test_that("calc_exceedance_prob applies the alpha plotting position constant", {
  result <- calc_exceedance_prob(c(10, 20, 30), alpha = 0.4)
  # ranks: 30=1, 20=2, 10=3; n=3; P = (rank - 0.4) / (n + 1 - 0.8)
  expect_equal(result, c(2.6, 1.6, 0.6) / 3.2)
})

test_that("calc_exceedance_prob errors on invalid alpha", {
  expect_snapshot(error = TRUE, calc_exceedance_prob(c(10, 20), alpha = 1))
  expect_snapshot(error = TRUE, calc_exceedance_prob(c(10, 20), alpha = "0.4"))
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

test_that("calc_exceedance_prob remove_zeros fills zero positions with NA", {
  result <- calc_exceedance_prob(c(10, 0, 30), remove_zeros = TRUE)
  expect_true(is.na(result[2]))
})

test_that("calc_exceedance_prob errors on non-numeric input", {
  expect_snapshot(error = TRUE, calc_exceedance_prob("abc"))
})

test_that("calc_exceedance_prob errors on invalid remove_zeros", {
  expect_snapshot(
    error = TRUE,
    calc_exceedance_prob(c(10, 20), remove_zeros = c(TRUE, FALSE))
  )
  expect_snapshot(
    error = TRUE,
    calc_exceedance_prob(c(10, 20), remove_zeros = NA)
  )
})

test_that("calc_exceedance_prob deprecates rm.zero argument", {
  expect_snapshot(invisible(calc_exceedance_prob(c(10, 0, 30), rm.zero = TRUE)))
})

test_that("calc_exceedance_prob treats Inf values as NA", {
  result <- calc_exceedance_prob(c(10, Inf, 30, -Inf))
  expect_equal(result, c(2 / 3, NA, 1 / 3, NA))
})

test_that("rcpp_calc_exceedance_prob matches R implementation", {
  flow <- c(10, 5, 0, 15, 8, NA, 0, 20, 10, Inf, -Inf)

  expect_equal(
    rcpp_calc_exceedance_prob(flow),
    calc_exceedance_prob(flow)
  )
  expect_equal(
    rcpp_calc_exceedance_prob(flow, remove_zeros = TRUE),
    calc_exceedance_prob(flow, remove_zeros = TRUE)
  )
  expect_equal(
    rcpp_calc_exceedance_prob(flow, alpha = 0.4),
    calc_exceedance_prob(flow, alpha = 0.4)
  )
})

test_that("rcpp_calc_exceedance_prob errors on invalid alpha", {
  expect_snapshot(error = TRUE, rcpp_calc_exceedance_prob(c(10, 20), alpha = 1))
})

test_that("rcpp_calc_exceedance_prob errors on invalid remove_zeros", {
  expect_snapshot(
    error = TRUE,
    rcpp_calc_exceedance_prob(c(10, 20), remove_zeros = NA)
  )
})

test_that("rcpp_calc_exceedance_prob deprecates rm.zero argument", {
  expect_snapshot(
    invisible(rcpp_calc_exceedance_prob(c(10, 0, 30), rm.zero = TRUE))
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
