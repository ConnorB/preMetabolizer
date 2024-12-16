#' Calculate Water Density
#'
#' This function calculates the density of water (kg/m^3) based on the temperature in degrees Celsius.
#' The equation is based on the formulation by Kell (1975) for pure water.
#'
#' @param tempC Numeric value representing the water temperature in degrees Celsius.
#'
#' @return A numeric value or vector representing the density of water in kg/m^3.
#'
#' @details
#' This function uses the polynomial approximation provided by Kell (1975) for the density of water
#' as a function of temperature. This approximation is valid for temperatures between 0°C and 150°C.
#'
#' @references
#' Kell, G. S. (1975). Density, thermal expansivity, and compressibility of liquid water from 0° to 150°C:
#' Correlations and tables for atmospheric pressure and saturation reviewed and expressed on 1968 temperature scale.
#' \emph{Journal of Chemical and Engineering Data}, 20(1), 97–105.
#' \doi{10.1021/je60064a005}
#'
#' @examples
#' # Example usage:
#' calc_water_density(tempC = 15)
#'
#' @export
calc_water_density <- function(tempC) {
  # Input validation
  if (!is.numeric(tempC) || any(is.na(tempC))) {
    stop("Input 'tempC' must be a numeric value or vector, without NA values.")
  }

  # Constants for water density calculation (Kell, 1975)
  a <- 999.83952
  b <- 16.945176
  c <- -7.9870401e-3
  d <- -46.170461e-6
  e <- 105.56302e-9
  f <- -280.54253e-12
  g <- 16.87985e-3

  # Calculate water density (kg/m^3)
  waterDensity <- (a + (b * tempC) + (c * tempC^2) + (d * tempC^3) +
                     (e * tempC^4) + (f * tempC^5)) / (1 + (g * tempC))

  return(waterDensity)
}
