test_that("convert_flow cfs to cms uses correct conversion factor", {
  expect_equal(convert_flow(1, "cfs", "cms"), 0.028316846592, tolerance = 1e-10)
})

test_that("convert_flow cms to lps multiplies by 1000", {
  expect_equal(convert_flow(1, "cms", "lps"), 1000)
  expect_equal(convert_flow(2.5, "cms", "lps"), 2500)
})

test_that("convert_flow same-unit conversion is identity", {
  expect_equal(convert_flow(42, "cfs", "cfs"), 42)
  expect_equal(convert_flow(42, "cms", "cms"), 42)
})

test_that("convert_flow is vectorized", {
  result <- convert_flow(c(10, 20, 30), "cfs", "cms")
  expect_length(result, 3)
  expect_equal(result[2], convert_flow(20, "cfs", "cms"))
})

test_that("convert_flow errors on invalid unit", {
  expect_snapshot(error = TRUE, convert_flow(1, "gallons", "cms"))
  expect_snapshot(error = TRUE, convert_flow(1, "cfs", "gallons"))
})
