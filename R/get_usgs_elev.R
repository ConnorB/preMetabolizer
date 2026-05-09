#' Get elevation from the USGS Elevation Point Query Service
#'
#' Queries the USGS Elevation Point Query Service for one or more latitude and
#' longitude pairs. Coordinates are always interpreted as WGS84 (WKID 4326).
#'
#' @param latitude,longitude Numeric vectors of latitude and longitude in decimal degrees.
#' @param units Unit for the returned elevation. Must be either `"Meters"` or
#'   `"Feet"`. Defaults to `"Meters"`.
#'
#' @return A numeric vector of elevations in the requested units, with one
#'   element for each input coordinate pair.
#'
#' @export
#' @importFrom cli cli_abort
get_usgs_elev <- function(latitude, longitude, units = c("Meters", "Feet")) {
  units <- rlang::arg_match(units)

  # Input validation
  if (
    !is.numeric(latitude) ||
      length(latitude) == 0 ||
      anyNA(latitude) ||
      any(!is.finite(latitude))
  ) {
    cli::cli_abort(
      "{.arg latitude} must be a numeric vector with finite values."
    )
  }
  if (
    !is.numeric(longitude) ||
      length(longitude) == 0 ||
      anyNA(longitude) ||
      any(!is.finite(longitude))
  ) {
    cli::cli_abort(
      "{.arg longitude} must be a numeric vector with finite values."
    )
  }
  if (length(latitude) != length(longitude)) {
    cli::cli_abort(
      "{.arg latitude} and {.arg longitude} must have the same length."
    )
  }
  if (any(latitude < -90 | latitude > 90)) {
    cli::cli_abort("{.arg latitude} values must be between -90 and 90.")
  }
  if (any(longitude < -180 | longitude > 180)) {
    cli::cli_abort("{.arg longitude} values must be between -180 and 180.")
  }

  elevations <- numeric(length(latitude))

  for (i in seq_along(latitude)) {
    response <- httr2::request("https://epqs.nationalmap.gov/v1/json") |>
      httr2::req_url_query(
        x = longitude[i],
        y = latitude[i],
        wkid = 4326,
        units = units,
        includeDate = "false"
      ) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_perform()
    result <- httr2::resp_body_json(response)

    # Convert to numeric - handles both numeric and character values
    elev <- suppressWarnings(as.numeric(result$value))

    if (is.null(result$value) || length(result$value) == 0 || is.na(elev)) {
      cli::cli_abort(
        "USGS elevation response did not include a valid elevation for point {i}."
      )
    }

    elevations[i] <- elev
  }

  elevations
}
