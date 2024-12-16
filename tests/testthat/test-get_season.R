library(testthat)

# Tests for get_season function
test_that("get_season works for valid dates", {
  expect_equal(get_season("2024-12-25"), "Winter")
  expect_equal(get_season("2024-07-04"), "Summer")
  expect_equal(get_season("2024-10-10"), "Fall")
  expect_equal(get_season("2024-03-21"), "Spring")
})

# Test for input as Date object
test_that("get_season works with Date objects", {
  expect_equal(get_season(as.Date("2024-12-25")), "Winter")
  expect_equal(get_season(as.Date("2024-06-21")), "Summer")
})

# Test for invalid input formats
test_that("get_season handles invalid inputs", {
  expect_error(get_season("InvalidDate"), "Error: Input is not a valid date or cannot be coerced to a Date.")
  expect_error(get_season(NA), "Error: Input is NA. Please provide a valid date.")
  expect_error(get_season(NULL), "Error: Input is NULL. Please provide a valid date.")
})

# Test for edge cases
test_that("get_season handles edge cases correctly", {
  expect_equal(get_season("2024-03-19"), "Winter")  # Last day of Winter
  expect_equal(get_season("2024-03-20"), "Spring")  # First day of Spring
  expect_equal(get_season("2024-06-20"), "Spring")  # Last day of Spring
  expect_equal(get_season("2024-06-21"), "Summer")  # First day of Summer
  expect_equal(get_season("2024-09-22"), "Summer")  # Last day of Summer
  expect_equal(get_season("2024-09-23"), "Fall")    # First day of Fall
  expect_equal(get_season("2024-12-20"), "Fall")    # Last day of Fall
  expect_equal(get_season("2024-12-21"), "Winter")  # First day of Winter
})
