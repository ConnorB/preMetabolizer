usgs_elev_cache <- new.env(parent = emptyenv())

#' Get elevation from the USGS Elevation Point Query Service
#'
#' Queries the USGS Elevation Point Query Service for one or more latitude and
#' longitude pairs. Coordinates are interpreted as WGS84 (WKID 4326). Requests
#' are performed in parallel for improved performance when querying multiple
#' points. Duplicated coordinate pairs are only queried once, and successful
#' results are cached for the rest of the R session, so repeated calls with
#' the same coordinates do not re-contact the service.
#'
#' @param latitude,longitude Numeric vectors of latitude and longitude in
#'   decimal degrees. Latitude values must be between -90 and 90; longitude
#'   values must be between -180 and 180. Western longitudes are negative.
#'   Inputs must be the same length.
#' @param units Character scalar specifying the output elevation units. Accepted
#'   values are `"meters"`, `"m"`, `"feet"`, and `"ft"`. Matching is case
#'   insensitive. Defaults to `"meters"`.
#' @param details Logical; if `FALSE` (the default), return a numeric elevation
#'   vector. If `TRUE`, return a tibble that also reports the USGS raster ID,
#'   raster resolution, source date when supplied by the service, and a status
#'   for each coordinate pair.
#'
#' @return A numeric vector of elevations in the requested units, with one
#'   element for each input coordinate pair. If an elevation cannot be retrieved
#'   for a point, `NA_real_` is returned for that location and one warning is
#'   issued for the failed points. With `details = TRUE`, a tibble is returned
#'   with the elevation and response metadata for each coordinate pair.
#'
#' @details
#' Elevations are interpolated from USGS 3DEP digital elevation models. They are
#' not surveyed elevations; accuracy varies with the source data at each
#' location. Use `details = TRUE` to retain the available raster metadata when
#' elevations need to be audited or compared.
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
  units = c("meters", "feet", "m", "ft"),
  details = FALSE
) {
  if (missing(units)) {
    units <- "meters"
  }
  check_string(units, allow_empty = FALSE)
  check_bool(details)

  units <- tolower(units)
  units <- rlang::arg_match(
    units,
    c("meters", "feet", "m", "ft")
  )

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

  keys <- paste(latitude, longitude, units, sep = "/")
  cached <- vapply(
    keys,
    \(key) exists(key, envir = usgs_elev_cache, inherits = FALSE),
    logical(1),
    USE.NAMES = FALSE
  )

  records <- vector("list", length(keys))
  for (i in which(cached)) {
    records[[i]] <- get(keys[[i]], envir = usgs_elev_cache)
  }

  # First occurrence of each coordinate pair not already cached
  fetch_idx <- which(!cached & !duplicated(keys))

  if (length(fetch_idx) == 0) {
    return(usgs_elev_result(records, latitude, longitude, units, details))
  }

  base_request <- httr2::request("https://epqs.nationalmap.gov/v1/json") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(30) |>
    httr2::req_retry(
      max_tries = 5,
      max_seconds = 60,
      retry_on_failure = TRUE,
      is_transient = \(response) {
        httr2::resp_status(response) %in% c(408, 425, 429, 500, 502, 503, 504)
      }
    )

  requests <- Map(
    \(lat, lon) {
      base_request |>
        httr2::req_url_query(
          x = lon,
          y = lat,
          wkid = 4326,
          units = units,
          includeDate = "true"
        )
    },
    latitude[fetch_idx],
    longitude[fetch_idx]
  )

  responses <- http_req_perform_parallel(
    requests,
    on_error = "continue",
    max_active = 4
  )

  parse_elevation <- function(response) {
    if (inherits(response, "error")) {
      if (inherits(response, "httr2_http")) {
        return(usgs_elev_record(
          status = "http_error",
          http_status = response$status,
          problem = paste0("USGS returned HTTP status ", response$status, ".")
        ))
      }

      return(usgs_elev_record(
        status = "request_error",
        problem = conditionMessage(response)
      ))
    }

    http_status <- httr2::resp_status(response)
    if (http_status < 200 || http_status >= 300) {
      return(usgs_elev_record(
        status = "http_error",
        http_status = http_status,
        problem = paste0("USGS returned HTTP status ", http_status, ".")
      ))
    }

    result <- tryCatch(
      httr2::resp_body_json(response, simplifyVector = FALSE),
      error = identity
    )

    if (inherits(result, "error")) {
      return(usgs_elev_record(
        status = "invalid_json",
        http_status = http_status,
        problem = conditionMessage(result)
      ))
    }

    if (!is.list(result) || is.null(result$value)) {
      return(usgs_elev_record(
        status = "invalid_response",
        http_status = http_status,
        problem = "USGS response did not include an elevation."
      ))
    }

    elev <- usgs_elev_number(result$value)

    if (!is.finite(elev)) {
      return(usgs_elev_record(
        status = "invalid_response",
        http_status = http_status,
        problem = "USGS response did not include a finite elevation."
      ))
    }

    usgs_elev_record(
      elevation = elev,
      raster_id = usgs_elev_number(result$rasterId),
      resolution = usgs_elev_number(result$resolution),
      source_date = usgs_elev_string(result$date),
      http_status = http_status
    )
  }

  values <- lapply(responses, parse_elevation)

  for (j in seq_along(fetch_idx)) {
    key <- keys[[fetch_idx[[j]]]]
    # Failed lookups are not cached so they can be retried in a later call
    if (identical(values[[j]]$status, "ok")) {
      assign(key, values[[j]], envir = usgs_elev_cache)
    }
    for (i in which(keys == key)) {
      records[[i]] <- values[[j]]
    }
  }

  usgs_elev_warn(records)
  usgs_elev_result(records, latitude, longitude, units, details)
}

usgs_elev_record <- function(
  elevation = NA_real_,
  raster_id = NA_real_,
  resolution = NA_real_,
  source_date = NA_character_,
  status = "ok",
  http_status = 200L,
  problem = NA_character_
) {
  list(
    elevation = elevation,
    raster_id = raster_id,
    resolution = resolution,
    source_date = source_date,
    status = status,
    http_status = http_status,
    problem = problem
  )
}

usgs_elev_number <- function(x) {
  if (
    (!is.numeric(x) && !is.character(x)) ||
      length(x) != 1 ||
      !is.null(names(x)) ||
      is.na(x)
  ) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(x))
  if (!is.finite(value)) {
    return(NA_real_)
  }

  value
}

usgs_elev_string <- function(x) {
  if (!is.character(x) || length(x) != 1 || !is.null(names(x)) || is.na(x)) {
    return(NA_character_)
  }

  x
}

usgs_elev_warn <- function(records) {
  failed <- which(vapply(records, \(record) record$status != "ok", logical(1)))

  if (length(failed) == 0) {
    return(invisible())
  }

  problems <- unique(vapply(
    records[failed],
    \(record) record$problem,
    character(1)
  ))
  problems <- utils::head(problems, 3)
  cli::cli_warn(
    c(
      "Could not retrieve USGS elevations for {length(failed)} point(s).",
      "i" = "Input positions: {toString(failed)}.",
      "x" = "{paste(problems, collapse = ' ')}"
    )
  )
}

usgs_elev_result <- function(records, latitude, longitude, units, details) {
  elevation <- vapply(records, \(record) record$elevation, numeric(1))

  if (!details) {
    return(elevation)
  }

  tibble::tibble(
    latitude = latitude,
    longitude = longitude,
    elevation = elevation,
    units = tolower(units),
    raster_id = vapply(records, \(record) record$raster_id, numeric(1)),
    resolution = vapply(records, \(record) record$resolution, numeric(1)),
    source_date = vapply(records, \(record) record$source_date, character(1)),
    status = vapply(records, \(record) record$status, character(1)),
    http_status = vapply(records, \(record) record$http_status, numeric(1)),
    problem = vapply(records, \(record) record$problem, character(1))
  )
}
