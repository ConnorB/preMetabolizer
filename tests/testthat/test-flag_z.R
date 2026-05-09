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
