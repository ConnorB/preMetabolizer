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
#' @param xCO2_ppm Numeric vector. Dry-air mole fraction of CO2 in parts per
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
#' [calc_CO2_molKg()].
#'
#' @references
#' Weiss, R.F., and Price, B.A. (1980). Nitrous oxide solubility in water and
#' seawater. Marine Chemistry, 8, 347-359.
#'
#' @seealso [calc_O2sat()], [calc_CH4sat()], [calc_N2Osat()], [calc_N2sat()],
#'   [calc_Arsat()], [calc_CO2_molKg()]
#'
#' @examples
#' calc_CO2sat(temp_water = 20, atmo_press = 1, salinity = 0)
#'
#' calc_CO2sat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = 101.325,
#'   units = "kPa",
#'   xCO2_ppm = 420,
#'   out_units = "mg/L"
#' )
#'
#' @export
calc_CO2sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xCO2_ppm = 420,
  out_units = "umol/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  check_numeric(xCO2_ppm)
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
  pressure_atm <- convert_pressure(atmo_press, from = units, to = "atm")
  P_H2O_atm <- calc_vapor_press(
    temp_water,
    salinity = salinity,
    method = "Dickson2007"
  )
  press_corr <- (pressure_atm - P_H2O_atm) / (1 - P_H2O_atm)
  CO2_molL <- F_co2 * xCO2_ppm * 1e-6 * press_corr
  if (out_units == "mg/L") {
    CO2_molL * 44.0095 * 1e3
  } else {
    CO2_molL * 1e6
  }
}

# Weiss & Price (1980) eq 13 solubility function F for a trace gas in moist air
# at 1 atm total pressure. Returns F in mol/(L atm) (volumetric) or mol/(kg atm)
# (gravimetric) depending on the supplied constants. Shared by calc_CO2sat() and
# calc_N2Osat().
weiss_price_F <- function(temp_water, salinity, A1, A2, A3, A4, B1, B2, B3) {
  abs_temp <- temp_water + 273.15
  temp_ratio <- abs_temp / 100
  ln_F <- A1 +
    A2 * (100 / abs_temp) +
    A3 * log(temp_ratio) +
    A4 * temp_ratio^2 +
    salinity * (B1 + B2 * temp_ratio + B3 * temp_ratio^2)
  exp(ln_F)
}
