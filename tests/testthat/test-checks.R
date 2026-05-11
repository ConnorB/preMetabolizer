test_that("check_bool accepts TRUE and FALSE", {
  expect_equal(check_bool(TRUE), TRUE)
  expect_equal(check_bool(FALSE), FALSE)
})

test_that("check_bool errors on non-logical, length != 1, or NA", {
  expect_snapshot(error = TRUE, check_bool("yes"))
  expect_snapshot(error = TRUE, check_bool(c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, check_bool(NA))
})

test_that("check_string accepts a single string", {
  expect_equal(check_string("a"), "a")
  expect_equal(check_string(""), "")
})

test_that("check_string allow_null returns NULL when permitted", {
  expect_null(check_string(NULL, allow_null = TRUE))
  expect_snapshot(error = TRUE, check_string(NULL))
})

test_that("check_string allow_empty = FALSE rejects empty string", {
  expect_snapshot(error = TRUE, check_string("", allow_empty = FALSE))
})

test_that("check_string errors on character vector or NA", {
  expect_snapshot(error = TRUE, check_string(c("a", "b")))
  expect_snapshot(error = TRUE, check_string(NA_character_))
  expect_snapshot(error = TRUE, check_string(1))
})

test_that("check_character accepts character vector", {
  expect_equal(check_character(c("a", "b")), c("a", "b"))
})

test_that("check_character allow_null returns NULL when permitted", {
  expect_null(check_character(NULL, allow_null = TRUE))
  expect_snapshot(error = TRUE, check_character(NULL))
})

test_that("check_character allow_na = FALSE rejects NA elements", {
  expect_snapshot(
    error = TRUE,
    check_character(c("a", NA), allow_na = FALSE)
  )
})

test_that("check_character allow_empty = FALSE rejects empty elements", {
  expect_snapshot(
    error = TRUE,
    check_character(c("a", ""), allow_empty = FALSE)
  )
})

test_that("check_character rejects non-character and zero-length input", {
  expect_snapshot(error = TRUE, check_character(1:3))
  expect_snapshot(error = TRUE, check_character(character()))
})

test_that("check_numeric accepts numeric vector", {
  expect_equal(check_numeric(c(1, 2, 3)), c(1, 2, 3))
  expect_equal(check_numeric(1L), 1L)
})

test_that("check_numeric allow_null returns NULL when permitted", {
  expect_null(check_numeric(NULL, allow_null = TRUE))
  expect_snapshot(error = TRUE, check_numeric(NULL))
})

test_that("check_numeric allow_na = FALSE rejects NA elements", {
  expect_snapshot(
    error = TRUE,
    check_numeric(c(1, NA), allow_na = FALSE)
  )
})

test_that("check_numeric default rejects infinite values", {
  expect_snapshot(error = TRUE, check_numeric(c(1, Inf)))
  expect_equal(check_numeric(c(1, Inf), allow_infinite = TRUE), c(1, Inf))
})

test_that("check_numeric rejects non-numeric input", {
  expect_snapshot(error = TRUE, check_numeric("1"))
})

test_that("cli_inform_if emits message only when condition is TRUE", {
  expect_message(cli_inform_if(TRUE, "hello"), "hello")
  expect_no_message(cli_inform_if(FALSE, "hello"))
  expect_no_message(cli_inform_if(NA, "hello"))
})
