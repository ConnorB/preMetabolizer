#' Calculate water density
#'
#' Calculates the density of pure water (kg/m^3) from temperature in
#' degrees Celsius using the polynomial approximation of Kell (1975).
#' Valid for temperatures between 0°C and 150°C at atmospheric pressure.
#'
#' @param water_temp Numeric vector. Water temperature in degrees Celsius.
#'
#' @return Numeric vector of water density in kg/m^3.
#'
#' @references
#' Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
#' liquid water from 0° to 150°C: Correlations and tables for atmospheric
#' pressure and saturation reviewed and expressed on 1968 temperature scale.
#' *Journal of Chemical and Engineering Data*, 20(1), 97–105.
#' \doi{10.1021/je60064a005}
#'
#' @examples
#' calc_water_density(15)
#' calc_water_density(c(0, 10, 20, 30))
#'
#' @export
calc_water_density <- function(water_temp) {
  if (!is.numeric(water_temp)) {
    cli::cli_abort("{.arg water_temp} must be numeric.")
  }

  if (any(water_temp < 0 | water_temp > 150, na.rm = TRUE)) {
    cli::cli_warn(c(
      "!" = "Some temperatures are outside the valid range.",
      "i" = "Valid range is 0-150\u00b0C."
    ))
  }

  a <- 999.83952
  b <- 16.945176
  c <- -7.9870401e-3
  d <- -46.170461e-6
  e <- 105.56302e-9
  f <- -280.54253e-12
  g <- 16.87985e-3

  density <-
    (a +
      b * water_temp +
      c * water_temp^2 +
      d * water_temp^3 +
      e * water_temp^4 +
      f * water_temp^5) /
    (1 + g * water_temp)

  density
}
