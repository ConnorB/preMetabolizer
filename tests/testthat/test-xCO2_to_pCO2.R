test_that("xCO2_to_pCO2 matches Dickson 2007 SOP 5 reference value", {
  # Section 8.5.3 of Guide to best practices for ocean CO2 measurements
  result <- xCO2_to_pCO2(378.16, 25, 1.0047, "atm", salinity = 35)
  expect_equal(result, 368.33, tolerance = 0.01)
})

test_that("xCO2_to_pCO2 and pCO2_to_xCO2 are inverse operations", {
  xco2 <- 400
  pco2 <- xCO2_to_pCO2(xco2, 25, 1, "atm")
  back <- pCO2_to_xCO2(25, pco2, 1, "atm")
  expect_equal(back, xco2, tolerance = 1e-6)
})

test_that("xCO2_to_pCO2 increases with xCO2_ppm", {
  low <- xCO2_to_pCO2(200, 25, 1, "atm")
  high <- xCO2_to_pCO2(800, 25, 1, "atm")
  expect_gt(high, low)
})

test_that("xCO2_to_pCO2 result is in µatm range for typical atmospheric CO2", {
  result <- xCO2_to_pCO2(420, 20, 1, "atm")
  expect_gt(result, 300)
  expect_lt(result, 500)
})
