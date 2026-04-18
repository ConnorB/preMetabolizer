#' Convert Barometric Pressure to Atmospheres
#'
#' Converts barometric pressure from various units to a target unit
#' using the `units` package.
#'
#' @param pressure Numeric or `units` object. Barometric pressure value(s) to be converted.
#' @param from Character. Units of the input barometric pressure. Accepted values are
#'   "atm", "hPa", "mbar", "kPa", "Pa", "Torr", "psi", and "bar". Ignored if `pressure` has units.
#' @param to Character. Target unit (default "atm").
#' @param .drop_units Logical. If `TRUE`, returns a numeric value instead of a `units` object.
#'
#' @return A `units` object or numeric, depending on `.drop_units`.
#'
#' @export
#' @importFrom units set_units as_units drop_units
convert_pressure <- function(pressure, from = NULL, to = "atm", .drop_units = FALSE) {
  unit_map <- c(
    atm = "atm",
    hpa = "hPa",
    mbar = "mbar",
    kpa = "kPa",
    pa = "Pa",
    torr = "torr",
    psi = "psi",
    bar = "bar"
  )

  from_lc <- tolower(from)
  to_lc <- tolower(to)

  if (!inherits(pressure, "units")) {
    if (is.null(from_lc) || !from_lc %in% names(unit_map)) {
      stop("If pressure is not a 'units' object, `from` must be one of: atm, hPa, mbar, kPa, Pa, Torr, psi, bar.")
    }
    pressure <- as_units(pressure, unit_map[[from_lc]])
  }

  if (!to_lc %in% names(unit_map)) {
    stop("Target unit must be one of: atm, hPa, mbar, kPa, Pa, Torr, psi, bar.")
  }

  result <- set_units(pressure, unit_map[[to_lc]], mode = "standard")

  if (.drop_units) drop_units(result) else result
}
