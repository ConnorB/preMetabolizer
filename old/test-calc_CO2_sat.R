# Load testthat
library(testthat)

# Test for calc_K0 function
test_that("calc_K0 calculates correct solubility constant", {
  # Test case 1: Known value at 20°C and 35 PSU
  K0 <- calc_K0(temp_water = 20, sal = 35)
  expect_equal(round(K0, 5), 3.241e-2)

  # Test case 2: Check edge case at -1°C and 0 PSU
  K0_zero <- calc_K0(temp_water = -1, sal = 0)
  expect_true(K0_zero > 0) # K0 should always be positive

  # Test case 3: Check temperature dependency
  K0_high_temp <- calc_K0(temp_water = 30, sal = 35)
  K0_low_temp <- calc_K0(temp_water = 10, sal = 35)
  expect_true(K0_high_temp < K0_low_temp) # K0 decreases with increasing temperature
})

test_that("calc_CO2_sat calculates correct CO2 saturation concentration", {

  # Test case 1: Known values at 25°C, 35 PSU, and 400 µatm pCO2
  #CO2_sat <- calc_CO2_sat(temp_water = 25, salinity = 35, pCO2_uatm = 400)
  #expected_CO2_sat <- calc_K0(25, 35) * (400 * 0.97) # Expected concentration
  #expect_equal(round(CO2_sat, 5), round(expected_CO2_sat, 5))

  # Test case 2: Zero salinity and 0 µatm pCO2
  CO2_sat_zero <- calc_CO2_sat(temp_water = 10, salinity = 0, pCO2_uatm = 0)
  expect_equal(CO2_sat_zero, 0) # No CO2 at zero partial pressure

  # Test case 3: Atmospheric pressure influence
  CO2_sat_high_press <- calc_CO2_sat(temp_water = 20, salinity = 35, pCO2_uatm = 400, atmo_press = 1)
  CO2_sat_low_press <- calc_CO2_sat(temp_water = 20, salinity = 35, pCO2_uatm = 400, atmo_press = 0.3)
  expect_true(CO2_sat_high_press > CO2_sat_low_press) # Higher pressure increases CO2 saturation

  # Test case 4: Salinity dependency
  CO2_sat_high_sal <- calc_CO2_sat(temp_water = 20, salinity = 40, pCO2_uatm = 400)
  CO2_sat_low_sal <- calc_CO2_sat(temp_water = 20, salinity = 0, pCO2_uatm = 400)
  expect_true(CO2_sat_high_sal < CO2_sat_low_sal) # Higher salinity reduces CO2 solubility
})
