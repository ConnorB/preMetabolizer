#' Calculate Water Density
#'
#' This function calculates the density of water (kg/m^3) based on the temperature in degrees Celsius.
#' The equation is based on the formulation by Kell (1975) for pure water.
#'
#' @param water_temp Numeric value representing the water temperature in degrees Celsius.
#' @param .drop_units Logical. If `TRUE`, returns a numeric value instead of a `units` object.
#'
#' @return A `units` object or numeric vector representing the density of water in kg/m^3.
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
#' calc_water_density(water_temp = 15)
#' calc_water_density(water_temp = 15, .drop_units = TRUE)
#'
#' @export
#' @importFrom units set_units drop_units
calc_water_density <- function(water_temp, .drop_units = TRUE) {
  if (!is.numeric(water_temp)) {
    stop("Input 'water_temp' must be a numeric value or vector.")
  }

  # Constants from Kell (1975)
  a <- 999.83952
  b <- 16.945176
  c <- -7.9870401e-3
  d <- -46.170461e-6
  e <- 105.56302e-9
  f <- -280.54253e-12
  g <- 16.87985e-3

  # Compute density
  rho <- (a + (b * water_temp) + (c * water_temp^2) + (d * water_temp^3) +
            (e * water_temp^4) + (f * water_temp^5)) / (1 + (g * water_temp))

  result <- set_units(rho, "kg/m^3")

  if (.drop_units) drop_units(result) else result
}
