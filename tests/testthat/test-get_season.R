test_that("get_season returns correct seasons for mid-season dates", {
  expect_equal(get_season("2024-01-15"), "Winter")
  expect_equal(get_season("2024-04-15"), "Spring")
  expect_equal(get_season("2024-07-04"), "Summer")
  expect_equal(get_season("2024-10-15"), "Fall")
})

test_that("get_season handles winter solstice boundary", {
  expect_equal(get_season("2024-12-21"), "Winter")
  expect_equal(get_season("2024-12-20"), "Fall")
})

test_that("get_season accepts Date objects", {
  expect_equal(get_season(as.Date("2024-07-04")), "Summer")
})

test_that("get_season is vectorized", {
  result <- get_season(c(
    "2024-01-01",
    "2024-04-01",
    "2024-07-01",
    "2024-10-01"
  ))
  expect_length(result, 4)
})

test_that("get_season errors on invalid input", {
  expect_snapshot(error = TRUE, get_season("not-a-date"))
})
