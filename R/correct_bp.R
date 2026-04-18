#' Correct Barometric Pressure for Elevation Change
#'
#' This function adjusts barometric pressure from one elevation to another using the barometric formula.
#' Input and output pressure units are flexible and handled via \code{\link{convert_pressure}}.
#'
#' @param station_bp A numeric vector of barometric pressures. Units assumed to be kPa unless specified via `from_units`.
#' @param air_temp A numeric vector of air temperatures at the station in degrees Celsius.
#' @param station_elev Elevation of the initial site in meters.
#' @param site_elev Elevation of the target site in meters.
#' @param from_units Character. Units of `station_bp`. Defaults to kPa.
#' @param to_units Character. Desired output units. Must be one of the same supported units.
#' @param drop_units Logical. If `TRUE`, returns a numeric vector instead of a `units` object.
#'
#' @return A vector of corrected barometric pressures at the new elevation in the specified units.
#'
#' @seealso \code{\link{convert_pressure}} for supported pressure units and unit conversion.
#'
#' @examples
#' correct_bp(station_bp = c(101.3, 100.5), air_temp = c(15, 10),
#'            station_elev = 300, site_elev = 500)
#' @importFrom units set_units drop_units
#' @export
correct_bp <- function(station_bp, air_temp, station_elev, site_elev,
                       from_units = "kPa", to_units = "kPa", drop_units = FALSE) {
  # Constants
  g <- 9.80665            # Gravity (m/s^2)
  R <- 8.3144598          # Universal gas constant (J/(mol K))
  air_molar_mass <- 0.0289644  # Molar mass of air (kg/mol)

  # Convert input pressure to Pa
  station_Pa <- convert_pressure(station_bp, from = from_units, to = "Pa", .drop_units = TRUE)

  # Convert temperature to Kelvin
  air_temp <- set_units(air_temp, "degree_Celsius")
  temp_K <- set_units(air_temp, "degree_kelvin")
  temp_K <- drop_units(temp_K)
  # Calculate elevation difference
  if (has_units(site_elev) || has_units(station_elev)) {
    delta_h <- drop_units(site_elev - station_elev)
  } else {
    delta_h <- site_elev - station_elev
  }

  # Adjust pressure using the barometric formula
  site_Pa <- station_Pa * exp(- (g * air_molar_mass * delta_h) / (R * temp_K))

  # Convert back to desired output units
  convert_pressure(site_Pa, from = "Pa", to = to_units, .drop_units = drop_units)
}
