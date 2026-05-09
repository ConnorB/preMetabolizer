#' Calculate Water Height from Sensor and Atmospheric Pressure
#'
#' This function computes the water height in meters based on the sensor pressure readings,
#' atmospheric pressure, water temperature, and sensor type (vented or unvented).
#'
#' @param sensor_kPa Numeric value representing the sensor pressure in kilopascals.
#'   - For `type = "vented"`, this is the differential pressure (water pressure only).
#'   - For `type = "unvented"`, this is the absolute pressure measured by the sensor.
#' @param atmo_kPa Numeric value representing the atmospheric pressure in kilopascals.
#'   Required when `type = "unvented"`.
#' @param water_temp Numeric value representing the water temperature in degrees Celsius.
#' @param type Character string specifying the sensor type: `"vented"` or `"unvented"`.
#'   Defaults to `"vented"`.
#'
#' @return A numeric value representing the water height in meters.
#'
#' @details
#' The function calculates the water density using the equation from Kell (1975) for pure water.
#' It then computes the water height by dividing the pressure difference by the product of
#' water density and gravity.
#'
#' - **Vented Sensor (`type = "vented"`):** The sensor measures the pressure difference directly,
#'   so only `sensor_kPa` is required.
#' - **Unvented Sensor (`type = "unvented"`):** The sensor measures absolute pressure,
#'   so atmospheric pressure (`atmo_kPa`) must be provided to calculate the pressure difference.
#'
#' @references
#' Kell, G. S. (1975). Density, thermal expansivity, and compressibility of liquid water from 0° to 150°C:
#' Correlations and tables for atmospheric pressure and saturation reviewed and expressed on 1968 temperature scale.
#' *Journal of Chemical and Engineering Data*, 20(1), 97–105.
#' \doi{10.1021/je60064a005}
#'
#' @examples
#' # Example usage with a vented sensor:
#' sensor_pressure <- 19.2 # kPa (pressure due to water column)
#' temperature <- 15 # degrees Celsius
#' calc_water_height(sensor_pressure, water_temp = temperature, type = "vented")
#'
#' # Example usage with an unvented sensor:
#' sensor_pressure <- 120.5 # kPa (absolute pressure)
#' atmospheric_pressure <- 101.3 # kPa
#' temperature <- 15 # degrees Celsius
#' calc_water_height(sensor_pressure,
#'   atmo_kPa = atmospheric_pressure,
#'   water_temp = temperature, type = "unvented"
#' )
#'
#' @export
calc_water_height <- function(
  sensor_kPa,
  atmo_kPa = NULL,
  water_temp,
  type = "vented"
) {
  # Input validation
  type <- match.arg(type, choices = c("vented", "unvented"))

  if (type == "unvented" && is.null(atmo_kPa)) {
    cli::cli_abort(
      "{.arg atmo_kPa} must be provided when {.arg type} is {.val unvented}."
    )
  }

  if (length(sensor_kPa) != length(water_temp)) {
    stop("Inputs 'sensor_kPa' and 'water_temp' must have the same length.")
  }

  # Calculate water density
  waterDensity <- calc_water_density(water_temp)

  # Calculate pressure difference in Pascals
  if (type == "vented") {
    delta_Pa <- sensor_kPa * 1000 # Sensor already measures differential pressure
  } else if (type == "unvented") {
    delta_Pa <- (sensor_kPa - atmo_kPa) * 1000 # Subtract atmospheric pressure
  }
  gravity <- 9.80665 # Gravity in m/s^2
  # Calculate water height in meters
  waterHeight_m <- delta_Pa / (waterDensity * gravity)

  return(waterHeight_m)
}
