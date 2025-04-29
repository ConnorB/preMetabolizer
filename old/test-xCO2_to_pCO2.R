library(testthat)

test_that("xCO2_to_pCO2 correctly calculates p(CO2) from section 8.5.3 of SOP 5 in Guide to best practices for ocean CO2 measurements", {
  # Expected result based on inputs
  expected_pCO2 <- 368.33

  # Example parameters
  calculated_pCO2 <- xCO2_to_pCO2(temp_water = 25, atmo_press = 1.0047, xCO2_ppm = 378.16, press_units = "atm", salinity = 35)

  # Check if result matches expected value within a small tolerance
  expect_equal(calculated_pCO2, expected_pCO2, tolerance = 0.01)
})
