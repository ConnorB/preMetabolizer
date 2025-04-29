#' Calculate Solubility Constant (K0) for CO2 in Water
#'
#' This function calculates the solubility constant (K0) for CO2 in water
#' as a function of temperature and salinity, based on Weiss (1974).
#'
#' @param temp_water Numeric. Water temperature in degrees Celsius.
#' @param waterDepth_m Numeric. Water depth in meters.
#' @param atmo_press Numeric. Atmospheric pressure in atm.
#' @param salinity Numeric. Salinity in PSU (Practical Salinity Units).
#'
#' @return Numeric. Solubility constant (K0) in mol/(kg·atm).
#' @references
#' Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility of
#' a non-ideal gas. Marine Chemistry, 2, 203-215.
#' @keywords internal
#' @export
calc_K0 <- function(temp_water, waterDepth_m = 0, atmo_press = 1, salinity = 0) {
  atmo_press_atm <- atmo_press

  # Convert water temperature to Kelvin
  abs_temp <- temp_water + 273.15

  # Constants for K0 calculation (Weiss, 1974), mol/(kg·atm)
  a1 <- -60.2409
  a2 <- 93.4517
  a3 <- 23.3585
  b1 <- 0.023517
  b2 <- -0.023656
  b3 <- 0.0047036

  # Calculate natural log of solubility coefficient K0
  ln_K0 <- a1 +
    a2 * (100 / abs_temp) +
    a3 * log(abs_temp / 100) +
    salinity * (b1 + b2 * (abs_temp / 100) + b3 * (abs_temp / 100)^2)

  # Calculate K0 in mol/(kg·atm)
  K0 <- exp(ln_K0)

  # Water pressure in atm (convert depth in meters to atm; 10.1325 m = 1 atm)
  water_press_atm <- waterDepth_m / 10.1325

  # Total pressure (atmospheric + water)
  total_pressure <- atmo_press_atm + water_press_atm

  # Constants for pressure correction
  R <- 82.05736 # Gas constant (cm³·atm)/(mol·K)
  pMolalVol_CO2 <- 32.3 # Partial molal volume of CO2 (cm^3/mol) from Weiss (1974, Appendix, paragraph 3)

  # Apply pressure correction to K0
  K0_corrected <- K0 * exp(((1 - total_pressure) * pMolalVol_CO2) / (R * abs_temp))

  return(K0_corrected) # mol/(kg·atm)
}
