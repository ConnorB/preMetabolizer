#' Calculate Dissolved Oxygen Saturation
#'
#' This function calculates dissolved oxygen saturation (\[DO\]) in water,
#' accounting for temperature, barometric pressure, and salinity.
#' It uses the Garcia and Gordon (1992) equations and corrects for vapor
#' pressure and water density.
#'
#' @param temp_water Numeric. Water temperature in degrees Celsius.
#' @param atmo_press Numeric. Barometric pressure values.
#' @param units Character. Units of barometric pressure. Accepted values are
#'   `"atm"`, `"hPa"`, `"mbar"`, and `"kPa"`.
#'   Default is `"atm"`.
#' @param salinity Numeric. salinity in parts per thousand (ppt). Default is `0`.
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
#' calc_O2sat(temp_water = temp, atmo_press = atmo_mbar, units = "mbar")
#'
#' # Example with kPa
#' temp <- 20
#' atmo_kPa <- c(101.3, 101.5, 101.2)  # Barometric pressure in kPa
#' calc_O2sat(temp_water = temp, atmo_press = atmo_kPa, units = "kPa")
#'
#' @export
calc_O2sat <- function(temp_water, atmo_press, units = "atm", salinity = 0) {
  # Barometric pressure conversion
  pressure_atm <- convert_pressure_to_atm(atmo_press, units)

  # Vapor pressure correction (Antoine equation)
  P_H2O_atm <- (exp(4.6543 - (1435.264 / ((temp_water + 273.15) + -64.848)))) * 0.98692  # Convert from bar to atm

  # Pressure correction [atm]
  press_corr <- (pressure_atm - P_H2O_atm) / (1 - P_H2O_atm)

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
  TS <- log((298.15 - temp_water) / (273.15 + temp_water))

  # salinity correction
  lnO2.sat <- A0 + A1 * TS + A2 * TS^2 + A3 * TS^3 + A4 * TS^4 + A5 * TS^5 +
    salinity * (B0 + B1 * TS + B2 * TS^2 + B3 * TS^3) + C0 * salinity^2

  # O2 saturation [umol/kg]
  DOsat_uMol.kg <- exp(lnO2.sat) * press_corr

  # Water density correction
  water_density <- calc_water_density(temp_water)

  # Convert from umol/kg to mg/L
  DOsat_mgL <- DOsat_uMol.kg * (10^(-6)) * 32 * water_density  # Molecular weight of O2 = 32 g/mol

  return(DOsat_mgL)
}
