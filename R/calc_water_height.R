#' Calculate Water Height from Sensor and Atmospheric Pressure
#'
#' This function computes the water height in meters based on the sensor pressure readings,
#' atmospheric pressure, water temperature, and sensor type (vented or unvented).
#'
#' @param sensor_kPa Numeric value representing the sensor pressure in kilopascals.
#'   - For \code{type = "vented"}, this is the differential pressure (water pressure only).
#'   - For \code{type = "unvented"}, this is the absolute pressure measured by the sensor.
#' @param atmo_kPa Numeric value representing the atmospheric pressure in kilopascals.
#'   Required when \code{type = "unvented"}.
#' @param tempC Numeric value representing the water temperature in degrees Celsius.
#' @param type Character string specifying the sensor type: \code{"vented"} or \code{"unvented"}.
#'   Defaults to \code{"vented"}.
#'
#' @return A numeric value representing the water height in meters.
#'
#' @details
#' The function calculates the water density using the equation from Kell (1975) for pure water.
#' It then computes the water height by dividing the pressure difference by the product of
#' water density and gravity.
#'
#' - **Vented Sensor (\code{type = "vented"}):** The sensor measures the pressure difference directly,
#'   so only \code{sensor_kPa} is required.
#' - **Unvented Sensor (\code{type = "unvented"}):** The sensor measures absolute pressure,
#'   so atmospheric pressure (\code{atmo_kPa}) must be provided to calculate the pressure difference.
#'
#' @references
#' Kell, G. S. (1975). Density, thermal expansivity, and compressibility of liquid water from 0° to 150°C:
#' Correlations and tables for atmospheric pressure and saturation reviewed and expressed on 1968 temperature scale.
#' \emph{Journal of Chemical and Engineering Data}, 20(1), 97–105.
#' \doi{10.1021/je60064a005}
#'
#' @examples
#' # Example usage with a vented sensor:
#' sensor_pressure <- 19.2 # kPa (pressure due to water column)
#' temperature <- 15 # degrees Celsius
#' calc_water_height(sensor_pressure, tempC = temperature, type = "vented")
#'
#' # Example usage with an unvented sensor:
#' sensor_pressure <- 120.5 # kPa (absolute pressure)
#' atmospheric_pressure <- 101.3 # kPa
#' temperature <- 15 # degrees Celsius
#' calc_water_height(sensor_pressure,
#'   atmo_kPa = atmospheric_pressure,
#'   tempC = temperature, type = "unvented"
#' )
#'
#' @export
calc_water_height <- function(sensor_kPa, atmo_kPa = NULL, tempC, type = "vented") {
  # Input validation
  type <- match.arg(type, choices = c("vented", "unvented"))

  if (!is.numeric(sensor_kPa) || any(is.na(sensor_kPa)) || any(sensor_kPa < 0)) {
    stop("Input 'sensor_kPa' must be a non-negative numeric value or vector, without NA values.")
  }
  if (!is.numeric(tempC) || any(is.na(tempC))) {
    stop("Input 'tempC' must be a numeric value or vector, without NA values.")
  }
  if (type == "unvented") {
    if (is.null(atmo_kPa)) {
      stop("For 'unvented' type, 'atmo_kPa' must be provided.")
    }
    if (!is.numeric(atmo_kPa) || any(is.na(atmo_kPa)) || any(atmo_kPa < 0)) {
      stop("Input 'atmo_kPa' must be a non-negative numeric value or vector, without NA values.")
    }
    if (length(atmo_kPa) != length(sensor_kPa)) {
      stop("Inputs 'sensor_kPa' and 'atmo_kPa' must have the same length when 'type' is 'unvented'.")
    }
  }
  if (length(sensor_kPa) != length(tempC)) {
    stop("Inputs 'sensor_kPa' and 'tempC' must have the same length.")
  }

  # Constants for water density calculation (Kell, 1975)
  a <- 999.83952
  b <- 16.945176
  c <- -7.9870401e-3
  d <- -46.170461e-6
  e <- 105.56302e-9
  f <- -280.54253e-12
  g <- 16.87985e-3
  gravity <- 9.80665 # Gravity in m/s^2

  # Calculate water density (kg/m^3)
  waterDensity <- (a + (b * tempC) + (c * tempC^2) + (d * tempC^3) +
                     (e * tempC^4) + (f * tempC^5)) / (1 + (g * tempC))

  # Calculate pressure difference in Pascals
  if (type == "vented") {
    delta_Pa <- sensor_kPa * 1000 # Sensor already measures differential pressure
  } else if (type == "unvented") {
    delta_Pa <- (sensor_kPa - atmo_kPa) * 1000 # Subtract atmospheric pressure
  }

  # Calculate water height in meters
  waterHeight_m <- delta_Pa / (waterDensity * gravity)

  return(waterHeight_m)
}
