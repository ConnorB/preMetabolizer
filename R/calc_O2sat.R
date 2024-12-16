#' Calculate Dissolved Oxygen Saturation
#'
#' This function calculates dissolved oxygen saturation (\[DO\]) in water,
#' accounting for temperature, barometric pressure, and salinity.
#' It uses the Garcia and Gordon (1992) equations and corrects for vapor
#' pressure and water density.
#'
#' @param tempC Numeric. Water temperature in degrees Celsius.
#' @param atmo_pres Numeric. Barometric pressure values.
#' @param units Character. Units of barometric pressure. Accepted values are
#'   `"atm"`, `"hPa"`, `"mbar"`, and `"kPa"`.
#'   Default is `"atm"`.
#' @param salinity Numeric. Salinity in parts per thousand (ppt). Default is `0`.
#'
#' @return Numeric. Dissolved oxygen saturation (\[DO\]) in mg/L.
#'
#' @details
#' The function calculates the dissolved oxygen saturation based on the Garcia
#' and Gordon (1992) model. It includes corrections for:
#' - Barometric pressure, converted from the specified units to atm.
#' - Vapor pressure of water, using the Antoine equation.
#' - Water density, dynamically calculated based on temperature.
#'
#' @examples
#' # Example with mbar
#' temp <- 25  # Water temperature in °C
#' atmo_mbar <- c(1013, 1015, 1012)  # Barometric pressure in mbar
#' calc_O2sat(tempC = temp, atmo_pres = atmo_mbar, units = "mbar")
#'
#' # Example with kPa
#' temp <- 20
#' atmo_kPa <- c(101.3, 101.5, 101.2)  # Barometric pressure in kPa
#' calc_O2sat(tempC = temp, atmo_pres = atmo_kPa, units = "kPa")
#'
#' @export
calc_O2sat <- function(tempC, atmo_pres, units = "atm", salinity = 0) {
  # Barometric pressure conversion
  if (units == "atm") {
    atmo_atm <- atmo_pres
  } else if (units == "hPa" || units == "mbar") {
    atmo_atm <- atmo_pres * 0.00098692316931427
  } else if (units == "kPa") {
    atmo_atm <- atmo_pres * 0.0098692316931427
  } else if (units == "Torr") {
    atmo_atm <- atmo_pres / 760
  } else if (units == "psi") {
    atmo_atm <- atmo_pres * 0.0680459639
  } else if (units == "bar") {
    atmo_atm <- atmo_pres * 0.98692316931427
  } else {
    stop("Please report barometric pressure in units of `atm`, `hPa`, `mbar`.")
  }

  # Vapor pressure correction (Antoine equation)
  vapor.press <- exp(4.6543 - (1435.264 / ((tempC + 273.15) + -64.848)))
  vapor.press <- vapor.press * 0.98692  # Convert from bar to atm

  # Pressure correction [atm]
  press.corr <- (atmo_atm - vapor.press) / (1 - vapor.press)

  # O2 saturation calculation (Garcia and Gordon 1992)
  A0 <- 5.80818
  A1 <- 3.20684
  A2 <- 4.1189
  A3 <- 4.93845
  A4 <- 1.01567
  A5 <- 1.41575
  B0 <- -7.01211 * 10^-3
  B1 <- -7.25958 * 10^-3
  B2 <- -7.93334 * 10^-3
  B3 <- -5.54491 * 10^-3
  C0 <- -1.32412 * 10^-7

  # Scaled temperature
  TS <- log((298.15 - tempC) / (273.15 + tempC))

  # Salinity correction
  lnO2.sat <- A0 + A1 * TS + A2 * TS^2 + A3 * TS^3 + A4 * TS^4 + A5 * TS^5 +
    salinity * (B0 + B1 * TS + B2 * TS^2 + B3 * TS^3) + C0 * salinity^2

  # O2 saturation [umol/kg]
  DOsat_uMol.kg <- exp(lnO2.sat) * press.corr

  # Water density correction
  waterDensity <- calc_water_density(tempC)

  # Convert from umol/kg to mg/L
  DOsat_mgL <- DOsat_uMol.kg * (10^(-6)) * 32 * waterDensity  # Molecular weight of O2 = 32 g/mol

  return(DOsat_mgL)
}
