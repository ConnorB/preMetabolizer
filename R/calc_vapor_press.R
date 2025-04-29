#' Calculate the vapor pressure of water at the supplied conditions
#'
#' This function calculates the vapor pressure of fresh or sea water.
#' @param temp_water Water temperature in degrees Celsius (default: 25).
#' @param salinity Salinity of the water in PSU (default: 0).
#' @param method Character. The method to use for calculation, either `"Dickson2007"` for seawater
#'        or `"MIMSY"` for freshwater. Defaults to `"Dickson2007"`.
#'
#' @return Vapor pressure in atm.
#' @details
#' - `"Dickson2007"` follows the method outlined in Dickson et al. (2007) and adjusts for seawater.
#' - `"MIMSY"` uses the Antoine equation for freshwater vapor pressure calculation.
#'
#' @references
#' Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to best practices for
#' ocean CO2 measurements. PICES Special Publication 3, 191 pp.
#'
#' Kelly, M.C. (2024). mimsy: Calculate MIMS Dissolved Gas Concentrations Without Getting a Headache.
#' R package version 0.6.5. Available at: \url{https://CRAN.R-project.org/package=mimsy}
#'
#' @examples
#' calc_vapor_press(temp_water = 25, salinity = 35,  method = "Dickson2007")
#' calc_vapor_press(temp_water = 20, salinity = 0, method = "MIMSY")
#' @export
calc_vapor_press <- function(temp_water = 25, salinity = 0, method = "Dickson2007") {
  kelvin_offset <- 273.15  # Conversion factor [deg C] to [K]
  abs_temp <- temp_water + kelvin_offset  # Absolute temperature [K]

  if (method == "Dickson2007") {
    # Calculate vapor pressure per Dickson, A.G., Sabine, C.L. and Christian, J.R. (Eds.) 2007. Guide to best practices for ocean CO2 measurements. PICES Special Publication 3, 191 pp.

    # Constants
    crit_temp <- 647.096  # [K]
    crit_press <- 22.064 / 101325.0e-6  # [atm], converted from MPa
    a1 <- -7.85951783
    a2 <- 1.84408259
    a3 <- -11.7866497
    a4 <- 22.6807411
    a5 <- -15.9618719
    a6 <- 1.80122502

    # Reduced temperature terms
    reduced_temp <- 1 - abs_temp / crit_temp
    zrt05 <- sqrt(reduced_temp)
    zrt15 <- reduced_temp * zrt05
    zrt2 <- reduced_temp^2
    zrt3 <- reduced_temp * zrt2
    zrt0 <- zrt3 * zrt05
    zrt4 <- zrt2^2
    zrt75 <- zrt4 * zrt0

    # Vapor pressure of water [atm]
    vp_water <- crit_press * exp((crit_temp / abs_temp) *
                                                      (a1 * reduced_temp + a2 * zrt15 + a3 * zrt3 + a4 * zrt0 + a5 * zrt4 + a6 * zrt75))

    # Constants for seawater correction
    co0 <- 0.90799
    co1 <- -0.08992
    co2 <- 0.18458
    co3 <- -0.07395
    co4 <- -0.00221

    ionic_strength <- 31.998 * salinity / (1000 - 1.005 * salinity)
    half_strength <- ionic_strength * 0.5
    zsmh2 <- half_strength^2
    zsmh3 <- half_strength * zsmh2
    zsmh4 <- zsmh2^2
    osmotic_press <- co0 + co1 * half_strength + co2 * zsmh2 + co3 * zsmh3 + co4 * zsmh4

    # Vapor pressure of seawater [atm]
    vapor_press <- vp_water * exp(-0.018 * osmotic_press * ionic_strength)

  } else if (method == "MIMSY") {
    if (salinity != 0) {
      warning("Salinity should be 0 when using the 'MIMSY' method")
    }
    # Antoine equation to calculate vapor pressure of water [bar]. See NIST Chemistry WebBook for coefficent table,
    # coefficents are valid for temperatures between -18 to 100C
    # Stull, D. R. (1947). Vapor Pressure of Pure Substances. Organic and Inorganic Compounds. Industrial & Engineering Chemistry, 39(4), 517–540. doi:10.1021/ie50448a022
    # Antoine equation parameters for freshwater (Stull, 1947)
    A <- 4.6543
    B <- 140.264
    C <- -64.848

    # Vapor pressure calculation (bar)
    vapor_press <- 10^(A - (B / (abs_temp + C)))  # [bar]

    # Convert bar to atm
    vapor_press <- vapor_press / 1.01325  # [atm]
  } else {
    stop("Invalid method. Choose 'Dickenson' or 'freshwater'.")
  }

  return(vapor_press)
}




