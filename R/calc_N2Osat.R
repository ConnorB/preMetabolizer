#' Calculate dissolved nitrous oxide saturation
#'
#' Calculates the dissolved nitrous oxide concentration in equilibrium with the
#' atmosphere from water temperature, barometric pressure, and salinity, using
#' the Weiss and Price (1980) solubility function for nitrous oxide as a
#' non-ideal atmospheric trace gas.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#' @param xN2O_ppm Numeric vector. Dry-air mole fraction of N2O in parts per
#'   million. Defaults to `0.338`, an approximate present-day value; override it
#'   with a measured atmospheric value where available.
#' @param out_units Character string. Output concentration units, either
#'   `"umol/L"` (the default) or `"mg/L"`.
#'
#' @return Numeric vector of dissolved N2O saturation in the requested units.
#'
#' @details
#' The equilibrium concentration is \eqn{C^* = x' F}, where \eqn{x'} is the
#' dry-air mole fraction and \eqn{F} is the moist-air solubility function of
#' Weiss and Price (1980), evaluated with their volumetric coefficients
#' (Table II). A vapor-pressure correction scales the 1 atm result to the
#' supplied barometric pressure.
#'
#' @references
#' Weiss, R.F., and Price, B.A. (1980). Nitrous oxide solubility in water and
#' seawater. Marine Chemistry, 8, 347-359.
#'
#' @seealso [calc_O2sat()], [calc_CO2sat()], [calc_CH4sat()]
#'
#' @examples
#' calc_N2Osat(temp_water = 20, atmo_press = 1, salinity = 0)
#'
#' calc_N2Osat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = 101.325,
#'   units = "kPa",
#'   xN2O_ppm = 0.338,
#'   out_units = "mg/L"
#' )
#'
#' @export
calc_N2Osat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xN2O_ppm = 0.338,
  out_units = "umol/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  check_numeric(xN2O_ppm)
  out_units <- rlang::arg_match(out_units, c("umol/L", "mg/L"))

  # Weiss & Price (1980) Table II volumetric constants [mol/(L atm)]
  F_n2o <- weiss_price_F(
    temp_water,
    salinity,
    A1 = -165.8806,
    A2 = 222.8743,
    A3 = 92.0792,
    A4 = -1.48425,
    B1 = -0.056235,
    B2 = 0.031619,
    B3 = -0.0048472
  )

  # C* = x' F at the Weiss & Price moist-air 1-atm reference condition,
  # then scaled by the dry-pressure ratio for the supplied barometric pressure.
  press_corr <- sat_press_corr(
    temp_water,
    atmo_press,
    units,
    salinity = salinity
  )
  N2O_molL <- F_n2o * xN2O_ppm * 1e-6 * press_corr

  if (out_units == "mg/L") {
    # mol/L -> mg/L (44.0128 g/mol N2O)
    N2O_molL * 44.0128 * 1e3
  } else {
    # mol/L -> umol/L
    N2O_molL * 1e6
  }
}
