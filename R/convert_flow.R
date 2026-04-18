#' Convert Stream Discharge to Various Units
#'
#' This function converts stream discharge measurements between cubic feet per second (cfs),
#' cubic meters per second (cms), and liters per second (lps). If the input does not have
#' units, it is assumed to be in cubic feet per second.
#'
#' @param flow A numeric vector or a `units` object of stream discharge. If numeric,
#'   it is assumed to be in cubic feet per second.
#' @param to A character string specifying the target unit. Acceptable values are
#'   "cfs" (cubic feet per second), "cms" (cubic meters per second), or "lps" (liters per second).
#'   Default is "cms".
#'
#' @return A `units` object of stream discharge in the specified unit.
#' @export
#' @importFrom units set_units as_units
#' @examples
#' convert_flow(flow = 14.32, to = "cms")  # Converts 14.32 cfs to cms
#' convert_flow(flow = 14.32, to = "lps")  # Converts 14.32 cfs to lps
#' convert_flow(flow = units::set_units(14.32, "ft^3/s"), to = "cms")  # With explicit units
#' convert_flow(flow = c(10, 20, 30), to = "cms")  # Converts multiple values from assumed cfs
convert_flow <- function(flow, to = "cms") {
  # Supported unit mapping
  unit_map <- c(cfs = "ft^3/s", cms = "m^3/s", lps = "l/s")

  # Validate target unit
  if (!to %in% names(unit_map)) {
    stop(sprintf("Invalid unit: '%s'. Choose one of: %s.", to, paste(names(unit_map), collapse = ", ")))
  }

  # Assign default units if missing
  if (!inherits(flow, "units")) {
    message("Flow has no units. Assuming default unit: 'ft^3/s'.")
    flow <- units::set_units(flow, "ft^3/s")
  }

  # Convert to target unit
  flow |>
    units::set_units(units::as_units(unit_map[[to]]), mode = "standard")
}

