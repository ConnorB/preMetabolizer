#' Calculate water height from pressure
#'
#' Converts pressure readings from vented or unvented water-level sensors to
#' water height in meters.
#'
#' @param sensor_kpa Numeric vector. Sensor pressure in kilopascals. For
#'   `type = "vented"`, this is differential pressure from the water column.
#'   For `type = "unvented"`, this is absolute pressure.
#' @param atmo_kpa Numeric vector. Atmospheric pressure in kilopascals. Required
#'   when `type = "unvented"`.
#' @param water_temp Numeric vector. Water temperature in degrees Celsius.
#' @param type Character string. Sensor type, either `"vented"` or
#'   `"unvented"`. Defaults to `"vented"`.
#' @param sensor_kPa `r lifecycle::badge("deprecated")` Use `sensor_kpa`
#'   instead.
#' @param atmo_kPa `r lifecycle::badge("deprecated")` Use `atmo_kpa` instead.
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
#' calc_water_height(sensor_kpa = 19.2, water_temp = 15, type = "vented")
#'
#' calc_water_height(
#'   sensor_kpa = 120.5,
#'   atmo_kpa = 101.3,
#'   water_temp = 15,
#'   type = "unvented"
#' )
#'
#' @export
calc_water_height <- function(
  sensor_kpa,
  atmo_kpa = NULL,
  water_temp,
  type = "vented",
  sensor_kPa = lifecycle::deprecated(),
  atmo_kPa = lifecycle::deprecated()
) {
  if (lifecycle::is_present(sensor_kPa)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "calc_water_height(sensor_kPa)",
      "calc_water_height(sensor_kpa)"
    )
    sensor_kpa <- sensor_kPa
  }
  if (lifecycle::is_present(atmo_kPa)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "calc_water_height(atmo_kPa)",
      "calc_water_height(atmo_kpa)"
    )
    atmo_kpa <- atmo_kPa
  }

  # Input validation
  type <- match.arg(type, choices = c("vented", "unvented"))

  if (type == "unvented" && is.null(atmo_kpa)) {
    cli::cli_abort(
      "{.arg atmo_kpa} must be provided when {.arg type} is {.val unvented}."
    )
  }

  if (length(sensor_kpa) != length(water_temp)) {
    cli::cli_abort(
      "{.arg sensor_kpa} and {.arg water_temp} must have the same length."
    )
  }

  # Calculate water density
  waterDensity <- calc_water_density(water_temp)

  # Calculate pressure difference in Pascals
  if (type == "vented") {
    delta_Pa <- sensor_kpa * 1000 # Sensor already measures differential pressure
  } else if (type == "unvented") {
    delta_Pa <- (sensor_kpa - atmo_kpa) * 1000 # Subtract atmospheric pressure
  }
  gravity <- 9.80665 # Gravity in m/s^2
  # Calculate water height in meters
  waterHeight_m <- delta_Pa / (waterDensity * gravity)

  return(waterHeight_m)
}
