set.seed(42)
x100 <- rnorm(100)

test_that("all methods return a positive numeric scalar", {
  methods <- c("auto", "sturges", "fd", "sqrt", "rice", "scott", "doane")
  for (m in methods) {
    result <- calc_bin_width(x100, m)
    expect_type(result, "double")
    expect_length(result, 1)
    expect_gt(result, 0)
  }
})

test_that("default method is auto", {
  expect_equal(calc_bin_width(x100), calc_bin_width(x100, "auto"))
})

test_that("NA values are silently removed", {
  x_na <- c(NA, x100, NA)
  expect_equal(calc_bin_width(x_na, "sturges"), calc_bin_width(x100, "sturges"))
})

test_that("fd falls back to sturges when IQR is zero", {
  x <- c(rep(0, 90), rep(1, 10))
  expect_equal(IQR(x), 0)
  expect_equal(calc_bin_width(x, "fd"), calc_bin_width(x, "sturges"))
})

test_that("non-numeric x errors", {
  expect_snapshot(calc_bin_width(letters), error = TRUE)
})

test_that("non-scalar method errors", {
  expect_snapshot(calc_bin_width(x100, c("fd", "sturges")), error = TRUE)
})

test_that("all-NA input errors", {
  expect_snapshot(calc_bin_width(c(NA_real_, NA_real_)), error = TRUE)
})

test_that("single non-NA value errors", {
  expect_snapshot(calc_bin_width(c(1, NA)), error = TRUE)
})

test_that("zero-range input errors", {
  expect_snapshot(calc_bin_width(rep(5, 10)), error = TRUE)
})

test_that("doane returns expected value for a skewed vector", {
  x <- c(1, 1, 1, 2, 2, 3, 5, 8, 13, 21)
  expect_equal(calc_bin_width(x, "doane"), 3.268178, tolerance = 1e-5)
})

test_that("doane with fewer than 3 observations errors", {
  expect_snapshot(calc_bin_width(c(1, 2), "doane"), error = TRUE)
})

test_that("unknown method errors", {
  expect_snapshot(calc_bin_width(x100, "badrule"), error = TRUE)
})
