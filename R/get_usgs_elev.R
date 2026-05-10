#' Get elevation from the USGS Elevation Point Query Service
#'
#' Queries the USGS Elevation Point Query Service for one or more latitude and
#' longitude pairs. Coordinates are interpreted as WGS84 (WKID 4326). Requests
#' are performed in parallel for improved performance when querying multiple
#' points.
#'
#' @param latitude,longitude Numeric vectors of latitude and longitude in
#'   decimal degrees. Latitude values must be between -90 and 90; longitude
#'   values must be between -180 and 180. Western longitudes are negative.
#'   Inputs must be the same length.
#' @param units Character scalar specifying the output elevation units. Accepted
#'   values are `"meters"`, `"m"`, `"feet"`, and `"ft"`. Matching is case
#'   insensitive. Defaults to `"meters"`.
#'
#' @return A numeric vector of elevations in the requested units, with one
#'   element for each input coordinate pair. If an elevation cannot be retrieved
#'   for a point, `NA_real_` is returned for that location and a warning is
#'   issued.
#'
#' @examples
#' \dontrun{
#' get_usgs_elev(
#'   latitude = 39.102075,
#'   longitude = -96.594689
#' )
#'
#' get_usgs_elev(
#'   latitude = c(39.102075, 38.8977),
#'   longitude = c(-96.594689, -77.0365),
#'   units = "ft"
#' )
#' }
#'
#' @export
get_usgs_elev <- function(
  latitude,
  longitude,
  units = c("meters", "feet", "m", "ft")
) {
  units <- match.arg(tolower(units[[1]]), c("meters", "feet", "m", "ft"))

  units <- switch(
    units,
    meters = "Meters",
    m = "Meters",
    feet = "Feet",
    ft = "Feet"
  )

  validate_coord <- function(x, name, min, max) {
    if (!is.numeric(x) || length(x) == 0) {
      cli::cli_abort("{.arg {name}} must be a non-empty numeric vector.")
    }

    bad <- is.na(x) | !is.finite(x) | x < min | x > max

    if (any(bad)) {
      cli::cli_abort(
        c(
          "{.arg {name}} contains invalid values.",
          "i" = "Values must be finite and between {min} and {max}.",
          "x" = "Invalid positions: {toString(which(bad))}"
        )
      )
    }

    invisible(x)
  }

  validate_coord(latitude, "latitude", -90, 90)
  validate_coord(longitude, "longitude", -180, 180)

  if (length(latitude) != length(longitude)) {
    cli::cli_abort(
      "{.arg latitude} and {.arg longitude} must have the same length."
    )
  }

  base_request <- httr2::request("https://epqs.nationalmap.gov/v1/json") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(10) |>
    httr2::req_retry(max_tries = 3)

  requests <- Map(
    \(lat, lon) {
      base_request |>
        httr2::req_url_query(
          x = lon,
          y = lat,
          wkid = 4326,
          units = units,
          includeDate = "false"
        )
    },
    latitude,
    longitude
  )

  responses <- http_req_perform_parallel(
    requests,
    on_error = "continue"
  )

  parse_elevation <- function(response, i) {
    if (inherits(response, "error")) {
      cli::cli_warn(
        c(
          "Failed to retrieve elevation for point {i}.",
          "x" = conditionMessage(response)
        )
      )
      return(NA_real_)
    }

    if (httr2::resp_status(response) >= 400) {
      cli::cli_warn(
        "USGS elevation request for point {i} returned HTTP status {httr2::resp_status(response)}."
      )
      return(NA_real_)
    }

    result <- tryCatch(
      httr2::resp_body_json(response, simplifyVector = TRUE),
      error = function(cnd) {
        cli::cli_warn(
          c(
            "Failed to parse elevation response for point {i}.",
            "x" = conditionMessage(cnd)
          )
        )
        NULL
      }
    )

    if (is.null(result) || is.null(result$value)) {
      cli::cli_warn(
        "USGS elevation response did not include an elevation for point {i}."
      )
      return(NA_real_)
    }

    elev <- suppressWarnings(as.numeric(result$value))

    if (length(elev) != 1 || is.na(elev)) {
      cli::cli_warn(
        "USGS elevation response did not include a valid elevation for point {i}."
      )
      return(NA_real_)
    }

    elev
  }

  Map(parse_elevation, responses, seq_along(responses)) |>
    unlist(use.names = FALSE)
}
