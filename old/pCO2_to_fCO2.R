#' Calculate Fugacity (fCO2) from Partial Pressure (pCO2)
#'
#' This function calculates the fugacity of carbon dioxide (fCO2) in water
#' from its partial pressure (pCO2) considering temperature, atmospheric pressure,
#' and hydrostatic pressure.
#'
#' @param temp_water Numeric. Water temperature in degrees Celsius. Default is 25.
#' @param atmo_press Numeric. Atmospheric pressure. Default is 1 atm.
#' @param hydro_press Numeric. Hydrostatic pressure. Default is 0 atm.
#' @param pCO2_uatm Numeric. Partial pressure of CO2 in micro-atmospheres (µatm).
#' @param press_units Character. Units of pressure, default is "atm".
#'
#' @return Numeric. Fugacity of CO2 in µatm.
#'
#' @details
#' This function uses the virial equation for real gases, as described by Weiss (1974),
#' to calculate the fugacity coefficient. The equation incorporates temperature corrections
#' and adjustments for non-ideal gas behavior based on empirical constants derived
#' from experimental data.
#'
#' @note The fugacity coefficient (\eqn{\phi}) is computed using the expression:
#' \deqn{\phi = \exp\left(\frac{P (B + 2 x^2 dCO2)}{R T}\right)}
#' where \eqn{P} is the total pressure, \eqn{B} is the virial coefficient of CO2,
#' \eqn{dCO2} is the cross virial coefficient for CO2-air interaction, \eqn{R}
#' is the gas constant, and \eqn{T} is the absolute temperature in Kelvin.
#'
#' @references
#' - Dickson, A.G., Sabine, C.L., & Christian, J.R. (2007). \emph{Guide to Best Practices for Ocean CO2 Measurements}.
#' - Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility of a non-ideal gas. Marine Chemistry, 2(3), 203–215.
#'
#' @examples
#' pCO2_to_fCO2(temp_water = 20, atmo_press = 1, hydro_press = 0.5, pCO2_uatm = 400)
#'
#' @export
pCO2_to_fCO2 <- function(temp_water = 25, atmo_press = 1, hydro_press = 0, pCO2_uatm, press_units = "atm") {
  # Constants
  kelvin_offset <- 273.15
  abs_temp_water <- temp_water + kelvin_offset  # Absolute temp_water [K]
  gas_const <- 8.31446261815324  # Gas constant [J/(mol K)]

  # Total pressure [atm]
  atmo_press_atm <- convert_pressure_to_atm(atmo_press, press_units)
  hydro_press_atm <- convert_pressure_to_atm(hydro_press, press_units)
  total_press <- atmo_press_atm + hydro_press_atm

  # Correct pressure for vapor pressure

  # Calculate vapor pressure correction
  #   pressure correction [atm] = (current pressure - vapor pressure) / (standard pressure [atm] - vapor pressure)
  vapor_press <- calc_vapor_press(salinity = 0, temp_water = temp_water, method = "Dickson2007")
  corr_press <- (total_press - vapor_press) / (1 - vapor_press)
  # Coefficients for fugacity calculation
  B <- -1636.75 + 12.0408 * abs_temp_water - 0.0327957 * abs_temp_water^2 + 0.0000316528 * abs_temp_water^3
  dCO2_Air <- 57.7 - 0.118 * abs_temp_water

  # Approximation for xCO2
  xCO2_approx <- pCO2_uatm
  x2 <- (1 - xCO2_approx * 1e-6)^2

  # Convert gas constant from J/(mol K) to cm^3 atm/(mol K)
  R <- gas_const / 0.101325  # [J/(mol K)] to [cm^3 atm/(mol K)]

  # Fugacity coefficient
  fugacity_coeff <- exp(corr_press * (B + 2 * x2 * dCO2_Air) / (R * abs_temp_water))
  fCO2_uatm <- pCO2_uatm * fugacity_coeff

  return(fCO2_uatm)
}

