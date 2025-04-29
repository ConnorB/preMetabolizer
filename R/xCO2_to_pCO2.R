#' Convert Mole Fraction of CO2 (xCO2) to Partial Pressure (pCO2)
#'
#' This function calculates the partial pressure of carbon dioxide (pCO2) in water
#' from its mole fraction (xCO2) in air, considering temperature and atmospheric pressure.
#'
#' @param xCO2_ppm Numeric. Mole fraction of CO2 in air in parts per million (ppm). Default is 400 ppm.
#' @param temp_water Numeric. Water temperature in degrees Celsius.
#' @param atmo_press Numeric. Atmospheric pressure.
#' @param press_units Character. Units of atmospheric pressure.
#' @param ... Additional arguments passed to the \code{calc_vapor_press} function.
#'
#' @return Numeric. Partial pressure of CO2 in µatm.
#'
#' @details
#' The partial pressure of CO2 (\eqn{pCO2}) is computed using the equation:
#' \deqn{pCO2 = (P_{atm} - P_{H2O}) \cdot xCO2}
#' where \eqn{P_{atm}} is the atmospheric pressure, \eqn{P_{H2O}} is the water vapor pressure,
#' and \eqn{xCO2} is the mole fraction of CO2 in air.
#'
#' @references
#' - Dickson, A.G., Sabine, C.L., & Christian, J.R. (2007). \emph{Guide to Best Practices for Ocean CO2 Measurements}.
#' - Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility of a non-ideal gas. Marine Chemistry, 2(3), 203–215.
#'
#' @keywords internal
#' @export
xCO2_to_pCO2 <- function(xCO2_ppm, temp_water, atmo_press, press_units, ...) {
  # Convert atmospheric pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(atmo_press, press_units)

  # Calculate vapor pressure of water
  vapor_press <- calc_vapor_press(temp_water, ...)

  # Calculate partial pressure of CO2
  pCO2_uatm <- (atmo_press_atm - vapor_press) * xCO2_ppm

  # Return pCO2 in uatm
  return(pCO2_uatm)
}

#' Convert p(CO2) to x(CO2)
#'
#' Converts partial pressure of CO2 p(CO2) µatm to mole fraction of CO2 x(CO2) ppm. This involves corrections for water vapor pressure to account for equilibrium conditions.
#'
#' @param temp_water Water temperature in degrees Celsius.
#' @param pCO2_uatm Partial pressure of CO2 in µatm.
#' @param atmo_press Atmospheric pressure.
#' @param press_units Units for pressure, either "atm" or others (default: "atm").
#' @param ... Additional parameters passed to `calc_vapor_press`.
#'
#' @return CO2 mole fraction (xCO2) in ppm.
#' @references
#' Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to best practices for
#' ocean CO2 measurements. PICES Special Publication 3, 191 pp.
#'
#' @keywords internal
#' @export
pCO2_to_xCO2 <- function(temp_water, pCO2_uatm, atmo_press, press_units, ...) {
  # Convert atmospheric pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(atmo_press, press_units)

  # Calculate vapor pressure of water
  vapor_press <- calc_vapor_press(temp_water, ...)

  # Calculate mole fraction of CO2 from partial pressure of CO2
  xCO2_ppm <- pCO2_uatm / (atmo_press_atm - vapor_press)
  return(xCO2_ppm)
}
