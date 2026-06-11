#' Calculate dissolved oxygen saturation
#'
#' Calculates the dissolved oxygen concentration in equilibrium with the
#' atmosphere from water temperature, barometric pressure, and salinity. This is
#' the `DO.sat` quantity commonly required by stream metabolism models.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#' @param out_units Character string. Output concentration units, either
#'   `"mg/L"` (the default) or `"umol/L"`.
#'
#' @return Numeric vector of dissolved oxygen saturation in the requested units.
#'
#' @details
#' The calculation uses the Benson and Krause umol/kg fit reported by Garcia
#' and Gordon (1992), applies a vapor-pressure correction for non-standard
#' barometric pressure, and converts from mass-based concentration to a volume
#' basis with a salinity-aware density correction.
#'
#' @references
#' Garcia, H.E., and Gordon, L.I. (1992). Oxygen solubility in seawater: better
#' fitting equations. Limnology and Oceanography, 37(6), 1307-1312.
#'
#' @seealso [calc_CO2sat()], [calc_CH4sat()], [calc_N2Osat()], [calc_N2sat()],
#'   [calc_Arsat()]
#'
#' @examples
#' calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
#'
#' calc_O2sat(
#'   temp_water = c(5, 15, 25),
#'   atmo_press = c(101.2, 100.8, 100.5),
#'   units = "kPa"
#' )
#'
#' # Return micromoles per liter instead of mg/L
#' calc_O2sat(temp_water = 15, atmo_press = 1, out_units = "umol/L")
#'
#' @export
calc_O2sat <- function(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  out_units = "mg/L"
) {
  check_numeric(temp_water)
  check_numeric(salinity)
  out_units <- rlang::arg_match(out_units, c("mg/L", "umol/L"))

  # Scale the 1-atm equilibrium concentration to the supplied barometric
  # pressure as C(P) = C(1 atm) * (P - pH2O) / (1 - pH2O), using the
  # salinity-aware Dickson et al. (2007) saturated water vapor pressure.
  press_corr <- sat_press_corr(atmo_press, units, temp_water, salinity)

  # O2 saturation calculation (Benson and Krause fit in Garcia and Gordon 1992)
  A0 <- 5.80871
  A1 <- 3.20291
  A2 <- 4.17887
  A3 <- 5.10006
  A4 <- -9.86643e-2
  A5 <- 3.80369
  B0 <- -7.01577e-3
  B1 <- -7.70028e-3
  B2 <- -1.13864e-2
  B3 <- -9.51519e-3
  C0 <- -2.75915e-7

  # Scaled temperature
  TS <- log((298.15 - temp_water) / (273.15 + temp_water))

  # salinity correction
  lnO2.sat <- A0 +
    A1 * TS +
    A2 * TS^2 +
    A3 * TS^3 +
    A4 * TS^4 +
    A5 * TS^5 +
    salinity * (B0 + B1 * TS + B2 * TS^2 + B3 * TS^3) +
    C0 * salinity^2

  # O2 saturation [umol/kg]
  DOsat_uMol.kg <- exp(lnO2.sat) * press_corr

  # Density [kg/m^3] used to convert from umol/kg to umol/L or mg/L
  water_density <- calc_water_density(temp_water, salinity)

  if (out_units == "umol/L") {
    # umol/kg * (kg/L) = umol/L
    DOsat_uMol.kg * water_density / 1000
  } else {
    # umol/kg -> mg/L (31.99880 g/mol O2; density in kg/m^3 gives g/m^3 = mg/L)
    DOsat_uMol.kg * (10^(-6)) * 31.99880 * water_density
  }
}
