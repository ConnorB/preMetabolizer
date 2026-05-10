#' Correct barometric pressure for elevation change
#'
#' Adjusts barometric pressure from one elevation to another using the
#' barometric formula. Always returns a plain numeric vector.
#'
#' @param station_bp Numeric. Barometric pressure at the station.
#' @param air_temp Numeric. Air temperature at the station in degrees Celsius.
#' @param station_elev Numeric. Elevation of the station in meters.
#' @param site_elev Numeric. Elevation of the target site in meters.
#' @param from_units Character. Units of `station_bp`. Defaults to `"kPa"`.
#'   See [convert_pressure()] for accepted values.
#' @param to_units Character. Desired output units. Defaults to `"kPa"`.
#'   See [convert_pressure()] for accepted values.
#'
#' @return Numeric vector of corrected barometric pressure in `to_units`.
#'
#' @seealso [convert_pressure()] for supported pressure units.
#'
#' @examples
#' correct_bp(
#'   station_bp = c(101.3, 100.5), air_temp = c(15, 10),
#'   station_elev = 300, site_elev = 500
#' )
#'
#' @export
correct_bp <- function(
  station_bp,
  air_temp,
  station_elev,
  site_elev,
  from_units = "kPa",
  to_units = "kPa"
) {
  g <- 9.80665
  R <- 8.3144598
  air_molar_mass <- 0.0289644

  station_Pa <- convert_pressure(station_bp, from = from_units, to = "Pa")
  temp_K <- air_temp + 273.15
  delta_h <- site_elev - station_elev

  site_Pa <- station_Pa * exp(-(g * air_molar_mass * delta_h) / (R * temp_K))

  convert_pressure(site_Pa, from = "Pa", to = to_units)
}
