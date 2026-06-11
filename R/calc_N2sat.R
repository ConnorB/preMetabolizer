#' Calculate dissolved nitrogen saturation
#'
#' Calculates the dissolved nitrogen (N2) concentration in equilibrium with
#' the atmosphere from water temperature, barometric pressure, and salinity,
#' using the Hamme and Emerson (2004) solubility fit for nitrogen in
#' air-equilibrated water.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#' @param out_units Character string. Output concentration units, either
#'   `"umol/L"` (the default) or `"mg/L"`.
#'
#' @return Numeric vector of dissolved N2 saturation in the requested units.
#'
#' @details
#' The equilibrium concentration follows the Hamme and Emerson (2004) eq 1 fit
#' with their Table 4 umol/kg coefficients, valid for water in equilibrium
#' with water-vapor-saturated air at 1 atm total pressure. A vapor-pressure
#' correction scales the 1 atm result to the supplied barometric pressure, and
#' a salinity-aware density correction converts from a mass to a volume basis.
#'
#' @references
#' Hamme, R.C., and Emerson, S.R. (2004). The solubility of neon, nitrogen and
#' argon in distilled water and seawater. Deep-Sea Research Part I, 51(11),
#' 1517-1528.
#'
#' @seealso [calc_Arsat()], [calc_O2sat()], [calc_CO2sat()], [calc_CH4sat()],
#'   [calc_N2Osat()]
#'
#' @examples
#' calc_N2sat(temp_water = 20, atmo_press = 1, salinity = 0)
#'
#' calc_N2sat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = 101.325,
#'   units = "kPa",
#'   out_units = "mg/L"
#' )
#'
#' @export
calc_N2sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  out_units = "umol/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  out_units <- rlang::arg_match(out_units, c("umol/L", "mg/L"))

  # Hamme & Emerson (2004) Table 4 umol/kg constants for N2
  N2_umolkg <- hamme_emerson_sat(
    temp_water,
    salinity,
    A0 = 6.42931,
    A1 = 2.92704,
    A2 = 4.32531,
    A3 = 4.69149,
    B0 = -7.44129e-3,
    B1 = -8.02566e-3,
    B2 = -1.46775e-2
  )

  # Equilibrium concentration for moist air at 1 atm, scaled to the supplied
  # barometric pressure.
  press_corr <- sat_press_corr(atmo_press, units, temp_water, salinity)
  N2_umolkg <- N2_umolkg * press_corr

  # Density [kg/m^3] used to convert from umol/kg to umol/L or mg/L
  water_density <- calc_water_density(temp_water, salinity)

  if (out_units == "mg/L") {
    # umol/kg -> mg/L (28.0134 g/mol N2; density in kg/m^3 gives g/m^3 = mg/L)
    N2_umolkg * 1e-6 * 28.0134 * water_density
  } else {
    # umol/kg * (kg/L) = umol/L
    N2_umolkg * water_density / 1000
  }
}
