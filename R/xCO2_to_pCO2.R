#' Convert CO2 mole fraction to partial pressure
#'
#' Converts CO2 mole fraction, `xCO2`, to the water-vapor-corrected partial
#' pressure, `pCO2`.
#'
#' @param xCO2_ppm Numeric vector. Mole fraction of CO2 in air in parts per
#'   million.
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Atmospheric pressure.
#' @param press_units Character string giving the units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units.
#' @param ... Additional arguments passed to [calc_vapor_press()], such as
#'   `salinity` or `method`.
#'
#' @return Numeric vector of partial pressure of CO2 in microatmospheres.
#'
#' @details
#' The conversion uses:
#' \deqn{pCO2 = (P_{atm} - P_{H2O}) \cdot xCO2}
#' where atmospheric pressure and water vapor pressure are in atm and `xCO2`
#' is supplied in parts per million.
#'
#' @references
#' Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
#' best practices for ocean CO2 measurements. PICES Special Publication 3.
#'
#' @examples
#' xCO2_to_pCO2(
#'   xCO2_ppm = 420,
#'   temp_water = 20,
#'   atmo_press = 101.325,
#'   press_units = "kPa",
#'   salinity = 0,
#'   method = "MIMSY"
#' )
#'
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

#' Convert CO2 partial pressure to mole fraction
#'
#' Converts water-vapor-corrected CO2 partial pressure, `pCO2`, to CO2 mole
#' fraction, `xCO2`.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param pCO2_uatm Numeric vector. Partial pressure of CO2 in
#'   microatmospheres.
#' @param atmo_press Numeric vector. Atmospheric pressure.
#' @param press_units Character string giving the units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units.
#' @param ... Additional arguments passed to [calc_vapor_press()], such as
#'   `salinity` or `method`.
#'
#' @return Numeric vector of CO2 mole fraction in parts per million.
#'
#' @references
#' Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
#' best practices for ocean CO2 measurements. PICES Special Publication 3.
#'
#' @examples
#' pCO2_to_xCO2(
#'   temp_water = 20,
#'   pCO2_uatm = 400,
#'   atmo_press = 101.325,
#'   press_units = "kPa",
#'   salinity = 0,
#'   method = "MIMSY"
#' )
#'
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
