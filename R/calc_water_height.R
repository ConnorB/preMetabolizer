#' Calculate water height from pressure
#'
#' Converts pressure readings from vented or unvented water-level sensors to
#' water height in meters.
#'
#' @param sensor_kPa Numeric vector. Sensor pressure in kilopascals. For
#'   `type = "vented"`, this is differential pressure from the water column.
#'   For `type = "unvented"`, this is absolute pressure.
#' @param atmo_kPa Numeric vector. Atmospheric pressure in kilopascals. Required
#'   when `type = "unvented"`.
#' @param water_temp Numeric vector. Water temperature in degrees Celsius.
#' @param type Character string. Sensor type, either `"vented"` or
#'   `"unvented"`. Defaults to `"vented"`.
#'
#' @return Numeric vector of water height in meters.
#'
#' @details
#' The pressure difference is divided by water density and gravitational
#' acceleration. Water density is calculated with [calc_water_density()].
#'
#' @references
#' Tanaka, M., Girard, G., Davis, R., Peuto, A., and Bignell, N. (2001).
#' Recommended table for the density of water between 0°C and 40°C based on
#' recent experimental reports. *Metrologia*, 38(4), 301-309.
#' \doi{10.1088/0026-1394/38/4/3}
#'
#' @examples
#' calc_water_height(sensor_kPa = 19.2, water_temp = 15, type = "vented")
#'
#' calc_water_height(
#'   sensor_kPa = 120.5,
#'   atmo_kPa = 101.3,
#'   water_temp = 15,
#'   type = "unvented"
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
    cli::cli_abort(
      "{.arg sensor_kPa} and {.arg water_temp} must have the same length."
    )
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
