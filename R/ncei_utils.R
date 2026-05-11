ncei_data_url <- "https://www.ncei.noaa.gov/access/services/data/v1"
ncei_search_url <- "https://www.ncei.noaa.gov/access/services/search/v1/data"
ncei_support_url <- "https://www.ncei.noaa.gov/access/services/support/v3/datasets"

ncei_request <- function(url) {
  httr2::request(url) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)
}

ncei_check_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (inherits(x, "Date") && length(x) == 1 && !is.na(x)) {
    return(format(x, "%Y-%m-%d"))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single date string or {.cls Date} object.",
      call = call
    )
  }
  parsed <- suppressWarnings(tryCatch(as.Date(x), error = function(e) NA))
  if (is.na(parsed)) {
    cli::cli_abort(
      "{.arg {arg}} must be a valid date in {.code YYYY-MM-DD} format.",
      call = call
    )
  }
  format(parsed, "%Y-%m-%d")
}

ncei_bbox_from_latlon <- function(latitude, longitude, dist_km) {
  lat_delta <- dist_km / 111.0
  lon_delta <- dist_km / (111.0 * cos(latitude * pi / 180))
  north <- min(latitude + lat_delta, 90)
  south <- max(latitude - lat_delta, -90)
  east <- min(longitude + lon_delta, 180)
  west <- max(longitude - lon_delta, -180)
  sprintf("%.6f,%.6f,%.6f,%.6f", north, west, south, east)
}

ncei_parse_stations <- function(body) {
  results <- body[["results"]]
  if (is.null(results)) {
    cli::cli_abort(
      c(
        "Unexpected response from the NCEI Search API.",
        "i" = "Expected a JSON object with a {.field results} field."
      )
    )
  }

  if (length(results) == 0) {
    return(tibble::tibble())
  }

  get_station_chr <- function(res, field) {
    sta <- res[["stations"]]
    if (is.null(sta) || length(sta) == 0) {
      return(NA_character_)
    }
    val <- sta[[1]][[field]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  get_date <- function(res, field) {
    val <- res[[field]]
    if (is.null(val)) as.Date(NA_character_) else as.Date(substr(val, 1, 10))
  }

  get_centroid <- function(res, idx) {
    pt <- res[["centroid"]][["point"]]
    if (is.null(pt) || length(pt) < 2) NA_real_ else as.numeric(pt[[idx]])
  }

  tibble::tibble(
    station_id = vapply(results, get_station_chr, character(1), field = "id"),
    name = vapply(results, get_station_chr, character(1), field = "name"),
    latitude = vapply(results, get_centroid, numeric(1), idx = 2),
    longitude = vapply(results, get_centroid, numeric(1), idx = 1),
    elevation = NA_real_,
    start_date = as.Date(vapply(
      results,
      \(r) {
        val <- r[["startDate"]]
        if (is.null(val)) NA_character_ else substr(val, 1, 10)
      },
      character(1)
    )),
    end_date = as.Date(vapply(
      results,
      \(r) {
        val <- r[["endDate"]]
        if (is.null(val)) NA_character_ else substr(val, 1, 10)
      },
      character(1)
    )),
    data_coverage = NA_real_
  )
}

ncei_stations_total <- function(body) {
  total <- body[["totalCount"]]
  if (is.null(total)) NA_integer_ else as.integer(total)
}
