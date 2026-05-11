test_that("flag_z detects clear outlier", {
  x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
  flags <- flag_z(x)
  expect_equal(flags[5], "Z")
})

test_that("flag_z returns NA for non-outliers", {
  x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
  flags <- flag_z(x)
  expect_true(is.na(flags[1]))
})

test_that("flag_z return_z = TRUE returns list with z and flag", {
  x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
  result <- flag_z(x, return_z = TRUE)
  expect_named(result, c("z", "flag"))
  expect_length(result$z, length(x))
  expect_length(result$flag, length(x))
})

test_that("flag_z output length equals input length", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  expect_length(flag_z(x), length(x))
})

test_that("flag_z does not flag uniform data", {
  x <- rep(5, 10)
  flags <- flag_z(x)
  expect_true(all(is.na(flags)))
})

test_that("flag_z z-score matches canonical biweight midvariance (c = 9)", {
  # Reference: Mosteller & Tukey (1977) / Lax (1985) biweight midvariance.
  biweight_scale <- function(r, c = 9) {
    mad_r <- stats::median(abs(r - stats::median(r)))
    u <- (r - stats::median(r)) / (c * mad_r)
    ok <- abs(u) < 1
    num <- sum((r[ok] - stats::median(r))^2 * (1 - u[ok]^2)^4)
    den <- sum((1 - u[ok]^2) * (1 - 5 * u[ok]^2))
    sqrt(length(r) * num) / abs(den)
  }
  x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
  result <- flag_z(x, return_z = TRUE)
  window <- x[3:7] # centered 5-wide window at i = 5
  expected_z <- (x[5] - stats::median(window)) / biweight_scale(window)
  expect_equal(result$z[5], expected_z, tolerance = 1e-6)
})
