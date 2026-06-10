#' Saturation vapor pressure of water
#'
#' Computes the saturation vapor pressure of water (or seawater) in atm.
#'
#' @param temp_water Water temperature in degrees Celsius. May be a vector.
#' @param salinity Practical salinity (unitless). Use 0 for freshwater.
#'   Ignored when `method = "MIMSY"`.
#' @param method Either "Dickson2007" (Wagner & Pruss pure-water fit with a
#'   seawater osmotic correction) or "MIMSY" (Antoine equation, freshwater).
#'   Defaults to `"Dickson2007"`.
#'
#' @return Saturation vapor pressure in atm.
#'
#' @references
#' Dickson, A.G., Sabine, C.L. and Christian, J.R. (Eds.) 2007. Guide to best
#'   practices for ocean CO2 measurements. PICES Special Publication 3, 191 pp.
#'   (Chapter 5, section 3.)
#' Stull, D.R. 1947. Vapor pressure of pure substances. Ind. Eng. Chem. 39(4):
#'   517-540. doi:10.1021/ie50448a022
#'Kelly, M.C. (2024). mimsy: Calculate MIMS Dissolved Gas Concentrations
#'   Without Getting a Headache. R package version 0.6.5.
#'   <https://CRAN.R-project.org/package=mimsy>
#'
#' @examples
#' calc_vapor_press(temp_water = 25, salinity = 35, method = "Dickson2007")
#' calc_vapor_press(temp_water = 20, salinity = 0, method = "MIMSY")
#'
#' @export
calc_vapor_press <- function(
  temp_water = 25,
  salinity = 0,
  method = "Dickson2007"
) {
  method <- rlang::arg_match(method, c("Dickson2007", "MIMSY"))
  abs_temp <- temp_water + 273.15 # absolute temperature [K]

  if (method == "Dickson2007") {
    # Pure water: Wagner & Pruss (2002), Dickson (2007) Ch.5 eq. (1)
    crit_temp <- 647.096 # critical temperature [K]
    crit_press <- 22.064 / 101325.0e-6 # critical pressure [atm], from MPa
    a1 <- -7.85951783
    a2 <- 1.84408259
    a3 <- -11.7866497
    a4 <- 22.6807411
    a5 <- -15.9618719
    a6 <- 1.80122502

    theta <- 1 - abs_temp / crit_temp # reduced temperature
    poly <- a1 *
      theta +
      a2 * theta^1.5 +
      a3 * theta^3 +
      a4 * theta^3.5 +
      a5 * theta^4 +
      a6 * theta^7.5
    vp_pure <- crit_press * exp((crit_temp / abs_temp) * poly) # [atm]

    # Seawater correction via osmotic coefficient, Dickson Ch.5 eqs. (2)-(4).
    # Reduces to 1 at salinity 0, so freshwater is unchanged.
    total_molality <- 31.998 * salinity / (1000 - 1.005 * salinity) # eq. (3)
    x <- 0.5 * total_molality
    osmotic_coef <- 0.90799 -
      0.08992 * x +
      0.18458 * x^2 -
      0.07395 * x^3 -
      0.00221 * x^4 # eq. (4)
    vapor_press <- vp_pure * exp(-0.018 * osmotic_coef * total_molality)
  } else {
    # Antoine equation, freshwater only. NIST coefficients (Stull 1947),
    # valid roughly -18 to 100 C.
    if (any(salinity != 0, na.rm = TRUE)) {
      cli::cli_warn(
        '{.arg salinity} is ignored when {.code method = "MIMSY"}.'
      )
    }
    A <- 4.6543
    B <- 1435.264
    C <- -64.848
    vapor_press <- 10^(A - B / (abs_temp + C)) / 1.01325 # [bar] -> [atm]
  }

  vapor_press
}
