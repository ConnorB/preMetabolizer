#' Calculate stream depth from USGS channel measurements
#'
#' Downloads channel measurements for a USGS gage and fits the at-a-station
#' hydraulic geometry relationship \eqn{D = cQ^f}. Mean measured depth is
#' calculated as channel area divided by channel width.
#'
#' @param gage_number Character scalar. USGS gage number, with or without the
#'   `"USGS-"` prefix.
#' @param discharge Numeric vector. Discharge in cubic feet per second (cfs).
#'   Values must be positive; missing values are returned as missing depths.
#'
#' @return A numeric vector of estimated mean stream depths in meters.
#'
#' @details
#' Channel measurements are retrieved with
#' [dataRetrieval::read_waterdata_channel()]. Measurements without positive
#' discharge, width, or area are omitted. Multiple channels recorded under one
#' measurement are combined before the power-law relationship is fitted.
#'
#' @references
#' Leopold, L. B., and Maddock, T. Jr. (1953). *The Hydraulic Geometry of
#' Stream Channels and Some Physiographic Implications*. USGS Professional
#' Paper 252. \doi{10.3133/pp252}
#'
#' @examples
#' \dontrun{
#' calc_usgs_depth("06879650", c(0.1, 1, 10))
#' }
#'
#' @export
calc_usgs_depth <- function(gage_number, discharge) {
  check_string(gage_number, allow_empty = FALSE)
  check_numeric(discharge)

  if (!grepl("^(USGS-)?[0-9]+$", gage_number)) {
    cli::cli_abort(
      "{.arg gage_number} must be a USGS gage number, optionally prefixed by {.val USGS-}."
    )
  }
  if (length(discharge) == 0L || any(discharge <= 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg discharge} must contain positive values.")
  }

  monitoring_location_id <- if (startsWith(gage_number, "USGS-")) {
    gage_number
  } else {
    paste0("USGS-", gage_number)
  }

  measurements <- read_usgs_channel(
    monitoring_location_id = monitoring_location_id,
    properties = c(
      "field_visit_id",
      "measurement_number",
      "channel_flow",
      "channel_flow_unit",
      "channel_width",
      "channel_width_unit",
      "channel_area",
      "channel_area_unit"
    ),
    skipGeometry = TRUE
  )

  required <- c(
    "field_visit_id",
    "measurement_number",
    "channel_flow",
    "channel_flow_unit",
    "channel_width",
    "channel_width_unit",
    "channel_area",
    "channel_area_unit"
  )
  if (!all(required %in% names(measurements))) {
    cli::cli_abort("USGS returned an unexpected channel-measurement response.")
  }

  usable <- with(
    measurements,
    is.finite(channel_flow) &
      channel_flow > 0 &
      channel_flow_unit == "ft^3/s" &
      is.finite(channel_width) &
      channel_width > 0 &
      channel_width_unit == "ft" &
      is.finite(channel_area) &
      channel_area > 0 &
      channel_area_unit == "ft^2"
  )
  usable <- !is.na(usable) & usable

  calibration <- measurements[usable, ] |>
    dplyr::group_by(.data$field_visit_id, .data$measurement_number) |>
    dplyr::summarise(
      discharge = sum(.data$channel_flow),
      depth = sum(.data$channel_area) / sum(.data$channel_width),
      .groups = "drop"
    )

  if (length(unique(calibration$discharge)) < 2L) {
    cli::cli_abort(
      "USGS has fewer than two usable channel measurements for gage {gage_number}."
    )
  }

  fit <- stats::lm(log(depth) ~ log(discharge), data = calibration)
  coefficients <- stats::coef(fit)

  exp(coefficients[[1]]) * discharge^coefficients[[2]] * 0.3048
}

read_usgs_channel <- function(...) {
  dataRetrieval::read_waterdata_channel(...)
}
