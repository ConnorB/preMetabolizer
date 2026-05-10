#' Convert stream discharge between units
#'
#' Converts stream discharge measurements between cubic feet per second (cfs),
#' cubic meters per second (cms), and liters per second (lps). Always returns
#' a plain numeric vector.
#'
#' @param flow Numeric. Stream discharge value(s) to be converted.
#' @param from Character. Units of the input discharge. Accepted values are
#'   `"cfs"` (cubic feet per second), `"cms"` (cubic meters per second), and
#'   `"lps"` (liters per second).
#' @param to Character. Target unit. Same accepted values as `from`. Defaults
#'   to `"cms"`.
#'
#' @return Numeric vector of stream discharge in the requested unit.
#'
#' @examples
#' convert_flow(14.32, from = "cfs", to = "cms")
#' convert_flow(14.32, from = "cfs", to = "lps")
#' convert_flow(c(10, 20, 30), from = "cfs", to = "cms")
#'
#' @export
convert_flow <- function(flow, from, to = "cms") {
  to_cms <- c(cfs = 0.028316846592, cms = 1, lps = 0.001)

  if (!from %in% names(to_cms)) {
    stop(sprintf(
      "`from` must be one of: %s.",
      paste(names(to_cms), collapse = ", ")
    ))
  }
  if (!to %in% names(to_cms)) {
    stop(sprintf(
      "`to` must be one of: %s.",
      paste(names(to_cms), collapse = ", ")
    ))
  }

  flow * to_cms[[from]] / to_cms[[to]]
}
