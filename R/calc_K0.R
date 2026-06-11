#' Calculate the CO2 solubility coefficient
#'
#' Calculates the Weiss (1974) CO2 solubility coefficient, `K0`, for water at
#' the supplied temperature, salinity, atmospheric pressure, and water depth.
#' The depth and atmospheric pressure terms apply a pressure correction for
#' the total pressure at the sensor.
#'
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param water_depth_m Numeric vector. Water depth in meters. Defaults to `0`.
#' @param atmo_press Numeric vector. Atmospheric pressure in atm. Defaults to
#'   standard atmosphere (`1`).
#' @param salinity Numeric vector. Salinity in practical salinity units.
#'   Defaults to freshwater (`0`).
#' @param waterDepth_m `r lifecycle::badge("deprecated")` Use `water_depth_m`
#'   instead.
#'
#' @return Numeric vector of `K0` values in mol/(kg atm).
#'
#' @references
#' Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility of
#' a non-ideal gas. Marine Chemistry, 2, 203-215.
#'
#' @examples
#' calc_K0(temp_water = 20)
#' calc_K0(temp_water = c(5, 15, 25), water_depth_m = 1, salinity = 0)
#'
#' @export
calc_K0 <- function(
  temp_water,
  water_depth_m = 0,
  atmo_press = 1,
  salinity = 0,
  waterDepth_m = lifecycle::deprecated()
) {
  if (lifecycle::is_present(waterDepth_m)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "calc_K0(waterDepth_m)",
      "calc_K0(water_depth_m)"
    )
    water_depth_m <- waterDepth_m
  }

  atmo_press_atm <- atmo_press

  # Convert water temperature to Kelvin
  abs_temp <- temp_water + 273.15

  # Constants for K0 calculation (Weiss, 1974), mol/(kg·atm)
  a1 <- -60.2409
  a2 <- 93.4517
  a3 <- 23.3585
  b1 <- 0.023517
  b2 <- -0.023656
  b3 <- 0.0047036

  # Calculate natural log of solubility coefficient K0
  ln_K0 <- a1 +
    a2 * (100 / abs_temp) +
    a3 * log(abs_temp / 100) +
    salinity * (b1 + b2 * (abs_temp / 100) + b3 * (abs_temp / 100)^2)

  # Calculate K0 in mol/(kg·atm)
  K0 <- exp(ln_K0)

  # Water pressure in atm (convert depth in meters to atm; 10.1325 m = 1 atm)
  water_press_atm <- water_depth_m / 10.1325

  # Total pressure (atmospheric + water)
  total_pressure <- atmo_press_atm + water_press_atm

  # Constants for pressure correction
  R <- 82.05736 # Gas constant (cm³·atm)/(mol·K)
  pMolalVol_CO2 <- 32.3 # Partial molal volume of CO2 (cm^3/mol) from Weiss (1974, Appendix, paragraph 3)

  # Apply pressure correction to K0
  K0_corrected <- K0 *
    exp(((1 - total_pressure) * pMolalVol_CO2) / (R * abs_temp))

  return(K0_corrected) # mol/(kg·atm)
}
