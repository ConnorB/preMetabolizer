test_that("xco2_to_pco2 matches Dickson 2007 SOP 5 reference value", {
  # Section 8.5.3 of Guide to best practices for ocean CO2 measurements
  result <- xco2_to_pco2(378.16, 25, 1.0047, "atm", salinity = 35)
  expect_equal(result, 368.33, tolerance = 0.01)
})

test_that("xco2_to_pco2 and pco2_to_xco2 are inverse operations", {
  xco2 <- 400
  pco2 <- xco2_to_pco2(xco2, 25, 1, "atm")
  back <- pco2_to_xco2(25, pco2, 1, "atm")
  expect_equal(back, xco2, tolerance = 1e-6)
})

test_that("xco2_to_pco2 increases with xco2_ppm", {
  low <- xco2_to_pco2(200, 25, 1, "atm")
  high <- xco2_to_pco2(800, 25, 1, "atm")
  expect_gt(high, low)
})

test_that("xco2_to_pco2 result is in µatm range for typical atmospheric CO2", {
  result <- xco2_to_pco2(420, 20, 1, "atm")
  expect_gt(result, 300)
  expect_lt(result, 500)
})

test_that("xCO2_to_pCO2() is deprecated", {
  expect_snapshot(invisible(xCO2_to_pCO2(420, 20, 1, "atm")))
})

test_that("pCO2_to_xCO2() is deprecated", {
  expect_snapshot(invisible(pCO2_to_xCO2(20, 400, 1, "atm")))
})
