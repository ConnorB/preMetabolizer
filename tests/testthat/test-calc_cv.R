test_that("calc_cv returns correct percentage value", {
  expect_equal(calc_cv(c(10, 20, 30, 40, 50)), 52.70463, tolerance = 1e-4)
})

test_that("calc_cv as_percent = FALSE returns ratio", {
  result <- calc_cv(c(10, 20, 30, 40, 50), as_percent = FALSE)
  expect_lt(result, 1)
  expect_equal(result * 100, calc_cv(c(10, 20, 30, 40, 50)))
})

test_that("calc_cv removes NA by default", {
  with_na <- calc_cv(c(10, 20, NA, 40, 50))
  without_na <- calc_cv(c(10, 20, 40, 50))
  expect_equal(with_na, without_na)
})

test_that("calc_cv returns NA when na.rm = FALSE and NA present", {
  expect_equal(calc_cv(c(10, 20, NA), na.rm = FALSE), NA_real_)
})

test_that("calc_cv warns on empty or zero-mean input", {
  expect_snapshot(calc_cv(numeric(0)))
  expect_snapshot(calc_cv(c(0, 0, 0)))
})

test_that("calc_cv robust uses median/MAD", {
  x <- c(1, 2, 3, 4, 100)
  robust <- calc_cv(x, robust = TRUE)
  standard <- calc_cv(x, robust = FALSE)
  expect_false(isTRUE(all.equal(robust, standard)))
})
