#' Calculate dissolved oxygen saturation
#'
#' Calculates dissolved oxygen saturation in mg/L from water temperature,
#' barometric pressure, and salinity. This is the `DO.sat` quantity commonly
#' required by stream metabolism models.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param atmo_press Numeric vector. Barometric pressure at the site.
#' @param units Character string. Units of `atmo_press`. Accepted values are
#'   `"atm"`, `"hPa"`, `"mbar"`, and `"kPa"`. Defaults to `"atm"`.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`).
#'
#' @return Numeric vector of dissolved oxygen saturation in mg/L.
#'
#' @details
#' The calculation uses the Benson and Krause umol/kg fit reported by Garcia
#' and Gordon (1992), applies a vapor-pressure correction for non-standard
#' barometric pressure, and converts from mass-based concentration to mg/L with
#' a salinity-aware density correction.
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
#' @export
calc_O2sat <- function(temp_water, atmo_press, units = "atm", salinity = 0) {
  # Barometric pressure conversion
  pressure_atm <- convert_pressure_to_atm(atmo_press, units)

  # Vapor pressure correction (Antoine equation)
  P_H2O_atm <- calc_vapor_press(temp_water, salinity = 0, method = "MIMSY")

  # Pressure correction [atm]
  press_corr <- (pressure_atm - P_H2O_atm) / (1 - P_H2O_atm)

  # O2 saturation calculation (Benson and Krause fit in Garcia and Gordon 1992)
  A0 <- 5.80871
  A1 <- 3.20291
  A2 <- 4.17887
  A3 <- 5.10006
  A4 <- -9.86643 * 10^-2
  A5 <- 3.80369
  B0 <- -7.01577 * 10^-3
  B1 <- -7.70028 * 10^-3
  B2 <- -1.13864 * 10^-2
  B3 <- -9.51519 * 10^-3
  C0 <- -2.75915 * 10^-7

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

  # Density correction from mass-based concentration to volume-based mg/L
  water_density <- calc_O2sat_density(temp_water, salinity)

  # Convert from umol/kg to mg/L
  DOsat_mgL <- DOsat_uMol.kg *
    (10^(-6)) *
    32 *
    water_density

  return(DOsat_mgL)
}

calc_O2sat_density <- function(temp_water, salinity) {
  density_fresh <- calc_water_density(temp_water)

  # Salinity terms from the UNESCO equation of state at atmospheric pressure.
  A <- 0.824493 -
    0.0040899 * temp_water +
    0.000076438 * temp_water^2 -
    0.00000082467 * temp_water^3 +
    0.0000000053875 * temp_water^4
  B <- -0.00572466 +
    0.00010227 * temp_water -
    0.0000016546 * temp_water^2
  C <- 0.00048314

  density_fresh +
    A * salinity +
    B * salinity^(3 / 2) +
    C * salinity^2
}
