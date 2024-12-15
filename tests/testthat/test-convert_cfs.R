library(testthat)

# Test cases for convert_cfs
test_that("convert_cfs converts CFS to CMS", {
  expect_equal(convert_cfs(14.32, to = "cms"), 14.32 * 0.028316847)
  expect_equal(convert_cfs(0, to = "cms"), 0)
  expect_equal(convert_cfs(c(10, 20, 30), to = "cms"), c(10, 20, 30) * 0.028316847)
})

test_that("convert_cfs converts CFS to L/s", {
  expect_equal(convert_cfs(14.32, to = "lps"), 14.32 * 0.028316847 * 1000)
  expect_equal(convert_cfs(0, to = "lps"), 0)
  expect_equal(convert_cfs(c(10, 20, 30), to = "lps"), c(10, 20, 30) * 0.028316847 * 1000)
})

test_that("convert_cfs handles invalid inputs", {
  expect_error(convert_cfs("string", to = "cms"), "Input must be a numeric vector.")
  expect_error(convert_cfs(14.32, to = "cfs"),
               'to must be either "cms" or "lps".')
})

test_that("convert_cfs handles edge cases", {
  expect_equal(convert_cfs(NA, to = "cms"), NA_real_)           # NA input
  expect_equal(convert_cfs(c(NA, 10), to = "cms"), c(NA, 10 * 0.028316847))  # Mixed input
})
