#' Calculate dissolved methane saturation
#'
#' Calculates the dissolved methane concentration in equilibrium with the
#' atmosphere from water temperature, barometric pressure, and salinity, using
#' the atmospheric-equilibrium solubility equation of Wiesenburg and Guinasso
#' (1979).
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#' @param xch4_ppm Numeric vector. Dry-air mole fraction of CH4 in parts per
#'   million. Defaults to `1.9`, an approximate present-day value; override it
#'   with a measured atmospheric value where available.
#' @param out_units Character string. Output concentration units, either
#'   `"umol/L"` (the default) or `"mg/L"`.
#'
#' @return Numeric vector of dissolved CH4 saturation in the requested units.
#'
#' @details
#' The equilibrium concentration follows Wiesenburg and Guinasso (1979) eq 7
#' with their volumetric (nmol/L) coefficients (Table VI), in which the dry-air
#' mole fraction enters the solubility equation directly. A vapor-pressure
#' correction scales the 1 atm result to the supplied barometric pressure.
#'
#' @references
#' Wiesenburg, D.A., and Guinasso, N.L. (1979). Equilibrium solubilities of
#' methane, carbon monoxide, and hydrogen in water and sea water. Journal of
#' Chemical and Engineering Data, 24(4), 356-360.
#'
#' @seealso [calc_o2_sat()], [calc_co2_sat()], [calc_n2o_sat()],
#'   [calc_n2_sat()], [calc_ar_sat()]
#'
#' @examples
#' calc_ch4_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#'
#' calc_ch4_sat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = 101.325,
#'   units = "kPa",
#'   xch4_ppm = 1.9,
#'   out_units = "mg/L"
#' )
#'
#' @export
calc_ch4_sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xch4_ppm = 1.9,
  out_units = "umol/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  check_numeric(xch4_ppm)
  out_units <- rlang::arg_match(out_units, c("umol/L", "mg/L"))

  abs_temp <- temp_water + 273.15
  temp_ratio <- abs_temp / 100
  x_frac <- xch4_ppm * 1e-6

  # Wiesenburg & Guinasso (1979) eq 7, Table VI nmol/L constants (the A4 term is
  # linear in T/100 and the mole fraction enters inside the logarithm).
  A1 <- -415.2807
  A2 <- 596.8104
  A3 <- 379.2599
  A4 <- -62.0757
  B1 <- -0.059160
  B2 <- 0.032174
  B3 <- -0.0048198

  ln_C <- log(x_frac) +
    A1 +
    A2 * (100 / abs_temp) +
    A3 * log(temp_ratio) +
    A4 * temp_ratio +
    salinity * (B1 + B2 * temp_ratio + B3 * temp_ratio^2)

  # Equilibrium concentration [nmol/L] at the moist-air 1-atm reference condition,
  # scaled to the supplied barometric pressure.
  press_corr <- sat_press_corr(atmo_press, units, temp_water, salinity)

  CH4_nmolL <- exp(ln_C) * press_corr
  if (out_units == "mg/L") {
    # nmol/L -> mg/L (16.0425 g/mol CH4)
    CH4_nmolL * 1e-9 * 16.0425 * 1e3
  } else {
    # nmol/L -> umol/L
    CH4_nmolL * 1e-3
  }
}

#' Calculate dissolved methane saturation (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [calc_ch4_sat()] instead.
#'
#' @examples
#' # Old:
#' # calc_CH4sat(temp_water = 20, atmo_press = 1, salinity = 0, xCH4_ppm = 1.9)
#' # New:
#' calc_ch4_sat(temp_water = 20, atmo_press = 1, salinity = 0, xch4_ppm = 1.9)
#'
#' @keywords internal
#' @export
calc_CH4sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xCH4_ppm = 1.9,
  out_units = "umol/L"
) {
  lifecycle::deprecate_soft("0.0.0.9000", "calc_CH4sat()", "calc_ch4_sat()")
  calc_ch4_sat(
    temp_water = temp_water,
    atmo_press = atmo_press,
    units = units,
    salinity = salinity,
    xch4_ppm = xCH4_ppm,
    out_units = out_units
  )
}
