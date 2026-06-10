#' Calculate water density
#'
#' Calculates the density of water (kg/m^3) from temperature in degrees Celsius
#' and salinity. For freshwater (`salinity = 0`) the density follows the
#' recommended pure-water (SMOW) equation of Tanaka et al. (2001) from 0°C to
#' 40°C, falling back to Kell (1975) above 40°C (valid to 150°C). For seawater
#' (nonzero salinity) it follows the one-atmosphere international equation of
#' state of Millero and Poisson (1981), valid from 0°C to 40°C and salinity 0.5
#' to 43. A warning is issued for temperatures or salinities outside the valid
#' range of the equation used.
#'
#' @param water_temp Numeric vector. Water temperature in degrees Celsius.
#' @param salinity Numeric vector. Salinity in parts per thousand. Defaults to
#'   freshwater (`0`). Any nonzero value uses the Millero and Poisson (1981)
#'   seawater equation.
#'
#' @return Numeric vector of water density in kg/m^3.
#'
#' @references
#' Tanaka, M., Girard, G., Davis, R., Peuto, A., and Bignell, N. (2001).
#' Recommended table for the density of water between 0°C and 40°C based on
#' recent experimental reports. *Metrologia*, 38(4), 301–309.
#' \doi{10.1088/0026-1394/38/4/3}
#'
#' Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
#' liquid water from 0° to 150°C: Correlations and tables for atmospheric
#' pressure and saturation reviewed and expressed on 1968 temperature scale.
#' *Journal of Chemical and Engineering Data*, 20(1), 97–105.
#' \doi{10.1021/je60064a005}
#'
#' Millero, F. J., and Poisson, A. (1981). International one-atmosphere equation
#' of state of seawater. *Deep-Sea Research Part A*, 28(6), 625–629.
#' \doi{10.1016/0198-0149(81)90122-9}
#'
#' @examples
#' calc_water_density(15)
#' calc_water_density(c(0, 10, 20, 30))
#'
#' # Seawater density at salinity 35
#' calc_water_density(15, salinity = 35)
#'
#' @export
calc_water_density <- function(water_temp, salinity = 0) {
  if (!is.numeric(water_temp)) {
    cli::cli_abort("{.arg water_temp} must be numeric.")
  }

  if (!is.numeric(salinity)) {
    cli::cli_abort("{.arg salinity} must be numeric.")
  }

  if (length(water_temp) == 0 || length(salinity) == 0) {
    return(numeric(0))
  }

  # Recycle to a common length so temperature and salinity can both be vectors.
  n <- max(length(water_temp), length(salinity))
  water_temp <- rep_len(water_temp, n)
  salinity <- rep_len(salinity, n)

  # Freshwater (salinity 0) uses Tanaka et al. (2001) up to 40°C and Kell (1975)
  # above 40°C; any nonzero salinity uses the Millero and Poisson (1981) seawater
  # equation.
  fresh <- !is.na(salinity) & salinity == 0

  if (any(fresh & (water_temp < 0 | water_temp > 150), na.rm = TRUE)) {
    cli::cli_warn(c(
      "!" = "Some temperatures are outside the valid range.",
      "i" = "Valid range is 0-150\u00b0C for freshwater (Tanaka et al. 2001 to 40\u00b0C, Kell 1975 above)."
    ))
  }
  if (any(!fresh & (water_temp < 0 | water_temp > 40), na.rm = TRUE)) {
    cli::cli_warn(c(
      "!" = "Some temperatures are outside the valid range.",
      "i" = "Valid range is 0-40\u00b0C for the Millero and Poisson (1981) seawater equation."
    ))
  }
  if (any(!fresh & (salinity < 0.5 | salinity > 43), na.rm = TRUE)) {
    cli::cli_warn(c(
      "!" = "Some salinities are outside the valid range.",
      "i" = "Valid range is 0.5-43 for the Millero and Poisson (1981) seawater equation."
    ))
  }

  # Freshwater above 40°C is outside Tanaka's fitted range, so use Kell there.
  use_kell <- fresh & !is.na(water_temp) & water_temp > 40
  use_tanaka <- fresh & !use_kell

  density <- numeric(n)
  density[use_tanaka] <- density_tanaka(water_temp[use_tanaka])
  density[use_kell] <- density_kell(water_temp[use_kell])
  density[!fresh] <- density_millero_poisson(
    water_temp[!fresh],
    salinity[!fresh]
  )
  density
}

# Tanaka et al. (2001) recommended density of de-aerated pure water (SMOW,
# kg/m^3) on the ITS-90 at 101325 Pa, valid 0-40°C. Thiesen's formula (their
# eq 1) with the five-parameter final fit from their Section 4.
density_tanaka <- function(water_temp) {
  a1 <- -3.983035
  a2 <- 301.797
  a3 <- 522528.9
  a4 <- 69.34881
  a5 <- 999.974950

  a5 *
    (1 - (water_temp + a1)^2 * (water_temp + a2) / (a3 * (water_temp + a4)))
}

# Kell (1975) eq 16: absolute density of pure water (kg/m^3) on the IPTS-68,
# valid 0-150°C at atmospheric pressure. Used for freshwater above 40°C, beyond
# the range of the Tanaka et al. (2001) fit.
density_kell <- function(water_temp) {
  a <- 999.83952
  b <- 16.945176
  c <- -7.9870401e-3
  d <- -46.170461e-6
  e <- 105.56302e-9
  f <- -280.54253e-12
  g <- 16.87985e-3

  (a +
    b * water_temp +
    c * water_temp^2 +
    d * water_temp^3 +
    e * water_temp^4 +
    f * water_temp^5) /
    (1 + g * water_temp)
}

# Millero & Poisson (1981) one-atmosphere international equation of state of
# seawater (kg/m^3), valid 0-40°C and salinity 0.5-43. The pure-water reference
# is the Bigg (1967) SMOW density (their eq 6); the salinity terms are their
# combined-data coefficients (eqs 5a-5c).
density_millero_poisson <- function(water_temp, salinity) {
  # SMOW reference density (Bigg 1967), Millero & Poisson (1981) eq 6
  rho_smow <- 999.842594 +
    6.793952e-2 * water_temp -
    9.095290e-3 * water_temp^2 +
    1.001685e-4 * water_temp^3 -
    1.120083e-6 * water_temp^4 +
    6.536336e-9 * water_temp^5

  # Salinity coefficients, Millero & Poisson (1981) eqs 5a-5c
  A <- 8.24493e-1 -
    4.0899e-3 * water_temp +
    7.6438e-5 * water_temp^2 -
    8.2467e-7 * water_temp^3 +
    5.3875e-9 * water_temp^4
  B <- -5.72466e-3 +
    1.0227e-4 * water_temp -
    1.6546e-6 * water_temp^2
  C <- 4.8314e-4

  rho_smow + A * salinity + B * salinity^(3 / 2) + C * salinity^2
}
