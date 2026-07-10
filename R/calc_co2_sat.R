#' Calculate dissolved carbon dioxide saturation
#'
#' Calculates the dissolved carbon dioxide concentration in equilibrium with the
#' atmosphere from water temperature, barometric pressure, and salinity, using
#' the Weiss and Price (1980) solubility function for carbon dioxide as a
#' non-ideal atmospheric trace gas.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#' @param xco2_ppm Numeric vector. Dry-air mole fraction of CO2 in parts per
#'   million. Defaults to `420`, an approximate present-day value; override it
#'   with a measured atmospheric value where available.
#' @param out_units Character string. Output concentration units, either
#'   `"umol/L"` (the default) or `"mg/L"`.
#'
#' @return Numeric vector of dissolved CO2 saturation in the requested units.
#'
#' @details
#' The equilibrium concentration is \eqn{C^* = x' F}, where \eqn{x'} is the
#' dry-air mole fraction and \eqn{F} is the moist-air solubility function of
#' Weiss and Price (1980), evaluated with their volumetric coefficients
#' (Table VI). A vapor-pressure correction scales the 1 atm result to the
#' supplied barometric pressure. To convert a *measured* CO2 mole fraction
#' (rather than the atmospheric value) to a dissolved concentration, see
#' [calc_co2_mol_kg()].
#'
#' @references
#' Weiss, R.F., and Price, B.A. (1980). Nitrous oxide solubility in water and
#' seawater. Marine Chemistry, 8, 347-359.
#'
#' @seealso [calc_o2_sat()], [calc_ch4_sat()], [calc_n2o_sat()],
#'   [calc_n2_sat()], [calc_ar_sat()], [calc_co2_mol_kg()]
#'
#' @examples
#' calc_co2_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#'
#' calc_co2_sat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = 101.325,
#'   units = "kPa",
#'   xco2_ppm = 420,
#'   out_units = "mg/L"
#' )
#'
#' @export
calc_co2_sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xco2_ppm = 420,
  out_units = "umol/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  check_numeric(xco2_ppm)
  out_units <- rlang::arg_match(out_units, c("umol/L", "mg/L"))

  # Weiss & Price (1980) Appendix, Table VI volumetric constants [mol/(L atm)]
  F_co2 <- weiss_price_F(
    temp_water,
    salinity,
    A1 = -160.7333,
    A2 = 215.4152,
    A3 = 89.8920,
    A4 = -1.47759,
    B1 = 0.029941,
    B2 = -0.027455,
    B3 = 0.0053407
  )
  # C* = x' F for moist air at 1 atm, scaled to the supplied barometric pressure
  press_corr <- sat_press_corr(atmo_press, units, temp_water, salinity)
  CO2_molL <- F_co2 * xco2_ppm * 1e-6 * press_corr
  if (out_units == "mg/L") {
    CO2_molL * 44.0095 * 1e3
  } else {
    CO2_molL * 1e6
  }
}

#' Calculate dissolved carbon dioxide saturation (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [calc_co2_sat()] instead.
#'
#' @examples
#' # Old:
#' # calc_CO2sat(temp_water = 20, atmo_press = 1, salinity = 0, xCO2_ppm = 420)
#' # New:
#' calc_co2_sat(temp_water = 20, atmo_press = 1, salinity = 0, xco2_ppm = 420)
#'
#' @keywords internal
#' @export
calc_CO2sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xCO2_ppm = 420,
  out_units = "umol/L"
) {
  lifecycle::deprecate_soft("0.0.0.9000", "calc_CO2sat()", "calc_co2_sat()")
  calc_co2_sat(
    temp_water = temp_water,
    atmo_press = atmo_press,
    units = units,
    salinity = salinity,
    xco2_ppm = xCO2_ppm,
    out_units = out_units
  )
}
