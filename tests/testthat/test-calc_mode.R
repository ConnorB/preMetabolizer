test_that("calc_mode returns single mode", {
  expect_equal(calc_mode(c(1, 2, 2, 3)), 2)
})

test_that("calc_mode multi = 'first' returns first mode", {
  expect_equal(calc_mode(c(1, 1, 2, 2, 3), multi = "first"), 1)
})

test_that("calc_mode multi = 'last' returns last mode", {
  expect_equal(calc_mode(c(1, 1, 2, 2, 3), multi = "last"), 2)
})

test_that("calc_mode multi = 'all' returns all modes", {
  result <- calc_mode(c(1, 1, 2, 2, 3), multi = "all")
  expect_length(result, 2)
})

test_that("calc_mode removes NA by default", {
  expect_equal(calc_mode(c(1, 2, 2, NA)), 2)
})

test_that("calc_mode returns NA for all-NA input", {
  expect_equal(calc_mode(c(NA, NA)), NA)
})

test_that("calc_mode returns NA for empty input", {
  expect_equal(calc_mode(integer(0)), NA)
})

test_that("calc_mode preserves numeric type", {
  result <- calc_mode(c(1.5, 2.5, 2.5))
  expect_type(result, "double")
})
