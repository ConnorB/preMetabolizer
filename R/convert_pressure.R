#' Convert barometric pressure between units
#'
#' Converts barometric pressure from one unit to another using exact
#' conversion factors. Always returns a plain numeric vector.
#'
#' @param pressure Numeric. Barometric pressure value(s) to be converted.
#' @param from Character. Units of the input barometric pressure. Accepted
#'   values are `"atm"`, `"hPa"`, `"mbar"`, `"kPa"`, `"Pa"`, `"Torr"`,
#'   `"psi"`, and `"bar"`.
#' @param to Character. Target unit. Same accepted values as `from`.
#'   Defaults to `"atm"`.
#'
#' @return Numeric vector of barometric pressure in the requested unit.
#'
#' @examples
#' convert_pressure(101.3, from = "kPa", to = "atm")
#' convert_pressure(1013.25, from = "hPa", to = "Pa")
#'
#' @export
convert_pressure <- function(pressure, from, to = "atm") {
  check_numeric(pressure)
  check_string(from, allow_empty = FALSE)
  check_string(to, allow_empty = FALSE)

  to_pa <- c(
    atm = 101325,
    hpa = 100,
    mbar = 100,
    kpa = 1000,
    pa = 1,
    torr = 133.322387415,
    psi = 6894.757293168,
    bar = 100000
  )

  from_lc <- tolower(from)
  to_lc <- tolower(to)

  valid <- names(to_pa)
  if (!from_lc %in% valid) {
    cli::cli_abort(
      "{.arg from} must be one of {.val {valid}}, not {.val {from}}."
    )
  }
  if (!to_lc %in% valid) {
    cli::cli_abort(
      "{.arg to} must be one of {.val {valid}}, not {.val {to}}."
    )
  }

  pressure * to_pa[[from_lc]] / to_pa[[to_lc]]
}
