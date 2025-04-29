#' Correct Barometric Pressure for Elevation Change
#'
#' This function adjusts barometric pressure from one elevation to another based on the barometric formula.
#'
#' @param station_kPa A vector of station barometric pressures in kPa.
#' @param air_temp A vector of air temperatures at the station in degrees Celsius.
#' @param station_elev Elevation of the initial site in meters.
#' @param site_elev Elevation of the target site in meters.
#'
#' @return A vector of corrected barometric pressures at the new elevation in kPa.
#' @examples
#' correct_bp(station_kPa = c(101.3, 100.5), air_temp = c(15, 10), station_elev = 300, site_elev = 500)
#'
#' @export
correct_bp <- function(station_kPa, air_temp, station_elev, site_elev) {
  # Constants
  g <- 9.80665 # Gravity (m/s^2)
  R <- 8.3144598 # Universal gas constant (J/(mol K))
  air_molar_mass <- 0.0289644 # Molar mass of air (kg/mol)

  # Convert temperature to Kelvin
  temp_K <- air_temp + 273.15

  # Convert station bp from kPa to Pa
  station_Pa <- station_kPa * 1000

  # Calculate the elevation difference
  delta_h <- site_elev - station_elev

  # Adjust pressure based on the elevation difference using the barometric formula
  site_Pa <- station_Pa * exp(- (g * air_molar_mass * delta_h) / (R * temp_K))

  # Convert Pa back to kPa for output
  site_kPa <- site_Pa / 1000

  return(site_kPa)
}
