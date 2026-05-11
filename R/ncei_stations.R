#' Search for NCEI weather stations
#'
#' Searches for stations in NOAA's National Centers for Environmental
#' Information (NCEI) using the Common Access Search Service API.
#'
#' @param dataset Character string. The dataset to search within, such as
#'   `"daily-summaries"` (GHCND) or `"global-hourly"` (ISD). See
#'   [ncei_datasets()] for more dataset identifiers.
#' @param bbox Optional numeric vector of length 4 specifying the
#'   geographic search area as `c(north, west, south, east)` in decimal
#'   degrees. North and south must be between -90 and 90; west and east
#'   between -180 and 180. When `NULL` no geographic filter is applied.
#' @param start_date,end_date Optional date range filter. Only stations
#'   with data overlapping this period are returned. Accepts `Date`
#'   objects or `"YYYY-MM-DD"` strings.
#' @param data_types Optional character vector of data type codes. Only
#'   stations that include all requested types are returned.
#' @param text Optional character string. Filters results to stations
#'   whose names contain this text.
#' @param limit Integer. Maximum number of stations to return per page
#'   (default 100, max 1000).
#' @param offset Integer. Zero-based pagination offset (default 0).
#'
#' @return A [tibble][tibble::tibble-package] with one row per station and
#'   the following columns:
#'   \describe{
#'     \item{`station_id`}{Station identifier (dataset prefix stripped,
#'       e.g., `"USW00023183"` rather than `"GHCND:USW00023183"`).}
#'     \item{`name`}{Station name.}
#'     \item{`latitude`,`longitude`}{Decimal-degree coordinates.}
#'     \item{`elevation`}{Elevation in metres.}
#'     \item{`start_date`,`end_date`}{Period of record as `Date` objects.}
#'     \item{`data_coverage`}{Fraction of expected observations present
#'       (0–1).}
#'   }
#'
#' @details
#' This function calls
#' `https://www.ncei.noaa.gov/access/services/search/v1/data`.
#'
#' To find stations near a specific point, compute a bounding box with
#' [ncei_bbox()] and pass it to `bbox`. For site-level station searches
#' prefer [closest_noaa_stations()], which computes the bounding box
#' automatically and filters by geodesic distance.
#'
#' @seealso [closest_noaa_stations()], [get_noaa_stations()],
#'   [ncei_bbox()], [ncei_data()]
#'
#' @examples
#' \dontrun{
#' # All daily-summaries stations in a region
#' ncei_stations(
#'   dataset = "daily-summaries",
#'   bbox = c(40, -100, 38, -98)
#' )
#'
#' # Hourly stations with temperature data and a long record
#' ncei_stations(
#'   dataset = "global-hourly",
#'   start_date = "1990-01-01",
#'   end_date = Sys.Date()
#' )
#' }
#'
#' @export
ncei_stations <- function(
  dataset,
  bbox = NULL,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  text = NULL,
  limit = 100L,
  offset = 0L
) {
  if (
    !is.character(dataset) ||
      length(dataset) != 1 ||
      is.na(dataset) ||
      !nzchar(dataset)
  ) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }
  if (!is.null(bbox)) {
    if (
      !is.numeric(bbox) ||
        length(bbox) != 4 ||
        anyNA(bbox) ||
        !all(is.finite(bbox))
    ) {
      cli::cli_abort(
        "{.arg bbox} must be a length-4 numeric vector: c(north, west, south, east)."
      )
    }
    if (bbox[1] < bbox[3]) {
      cli::cli_abort(
        "{.arg bbox}: north ({bbox[1]}) must be >= south ({bbox[3]})."
      )
    }
  }
  if (!is.null(start_date)) {
    start_date <- ncei_check_date(start_date)
  }
  if (!is.null(end_date)) {
    end_date <- ncei_check_date(end_date)
  }
  if (!is.null(data_types)) {
    if (
      !is.character(data_types) || length(data_types) == 0 || anyNA(data_types)
    ) {
      cli::cli_abort("{.arg data_types} must be a character vector or `NULL`.")
    }
  }
  if (!is.null(text)) {
    if (!is.character(text) || length(text) != 1 || is.na(text)) {
      cli::cli_abort("{.arg text} must be a single string or `NULL`.")
    }
  }
  if (
    !is.numeric(limit) ||
      length(limit) != 1 ||
      is.na(limit) ||
      limit < 1 ||
      limit > 1000 ||
      limit != as.integer(limit)
  ) {
    cli::cli_abort("{.arg limit} must be a whole number between 1 and 1000.")
  }
  if (
    !is.numeric(offset) ||
      length(offset) != 1 ||
      is.na(offset) ||
      offset < 0 ||
      offset != as.integer(offset)
  ) {
    cli::cli_abort("{.arg offset} must be a non-negative whole number.")
  }

  bbox_str <- if (!is.null(bbox)) {
    sprintf("%.6f,%.6f,%.6f,%.6f", bbox[1], bbox[2], bbox[3], bbox[4])
  } else {
    NULL
  }

  req <- ncei_request(ncei_search_url) |>
    httr2::req_url_query(
      dataset = dataset,
      limit = as.integer(limit),
      offset = as.integer(offset)
    )

  if (!is.null(bbox_str)) {
    req <- httr2::req_url_query(req, bbox = bbox_str)
  }
  if (!is.null(start_date)) {
    req <- httr2::req_url_query(req, startDate = start_date)
  }
  if (!is.null(end_date)) {
    req <- httr2::req_url_query(req, endDate = end_date)
  }
  if (!is.null(data_types)) {
    req <- httr2::req_url_query(
      req,
      dataTypes = paste(data_types, collapse = ",")
    )
  }
  if (!is.null(text)) {
    req <- httr2::req_url_query(req, text = text)
  }

  tryCatch(
    {
      resp <- req |> http_req_perform()
      body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      ncei_parse_stations(body)
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to retrieve stations from the NCEI Search API.",
        parent = e
      )
    }
  )
}

#' Compute a bounding box around a point
#'
#' Returns a bounding box vector suitable for the `bbox` argument of
#' [ncei_stations()] and [get_noaa_stations()].
#'
#' @param latitude,longitude Numeric. Target coordinates in decimal
#'   degrees. Latitude must be between -90 and 90; longitude between
#'   -180 and 180.
#' @param dist_km Positive numeric. Half-width of the bounding box in
#'   kilometres.
#'
#' @return A numeric vector `c(north, west, south, east)`.
#'
#' @examples
#' # Bounding box 50 km around Konza Prairie
#' ncei_bbox(39.1, -96.6, 50)
#'
#' @export
ncei_bbox <- function(latitude, longitude, dist_km) {
  if (
    !is.numeric(latitude) ||
      length(latitude) != 1 ||
      is.na(latitude) ||
      abs(latitude) > 90
  ) {
    cli::cli_abort(
      "{.arg latitude} must be a single number between -90 and 90."
    )
  }
  if (
    !is.numeric(longitude) ||
      length(longitude) != 1 ||
      is.na(longitude) ||
      abs(longitude) > 180
  ) {
    cli::cli_abort(
      "{.arg longitude} must be a single number between -180 and 180."
    )
  }
  if (
    !is.numeric(dist_km) ||
      length(dist_km) != 1 ||
      is.na(dist_km) ||
      dist_km <= 0
  ) {
    cli::cli_abort("{.arg dist_km} must be a single positive number.")
  }

  lat_delta <- dist_km / 111.0
  lon_delta <- dist_km / (111.0 * cos(latitude * pi / 180))

  c(
    north = min(latitude + lat_delta, 90),
    west = max(longitude - lon_delta, -180),
    south = max(latitude - lat_delta, -90),
    east = min(longitude + lon_delta, 180)
  )
}
