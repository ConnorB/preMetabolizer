library(testthat)

test_that("pCO2_to_fCO2 correctly calculates f(CO2) from section 8.5.5 of SOP 5 in Guide to best practices for ocean CO2 measurements", {
  # Expected result based on inputs
  expected_fCO2 <- 367.15

  # Example parameters
  calculated_fCO2 <- pCO2_to_fCO2(temp_water = 25, atmo_press = 1, hydro_press = 0, pCO2_uatm = 368.33, press_units = "atm")

  # Check if result matches expected value within a small tolerance
  expect_equal(calculated_fCO2, expected_fCO2, tolerance = 0.01)
})
