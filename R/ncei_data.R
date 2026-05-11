#' Get data from the NCEI Data Service API
#'
#' Retrieves weather and climate data from NOAA's National Centers for
#' Environmental Information (NCEI) using the Access Data Service API.
#'
#' @param dataset Character string. The dataset to query. Common values
#'   include `"daily-summaries"` (GHCND), `"global-hourly"` (ISD),
#'   `"global-summary-of-the-month"`, and `"global-summary-of-the-year"`.
#'   New datasets are added periodically; use [ncei_datasets()] to list
#'   what is currently available.
#' @param stations Character vector of station identifiers, such as those
#'   returned by [ncei_stations()] or [get_noaa_stations()].
#' @param start_date,end_date Start and end of the requested period, as
#'   `Date` objects or `"YYYY-MM-DD"` strings.
#' @param data_types Optional character vector of data type codes to
#'   include (e.g., `c("TMAX", "TMIN", "PRCP")` for daily summaries).
#'   When `NULL` all available types are returned.
#' @param units Character string. `"metric"` (default) or `"standard"`.
#' @param include_station_name Logical. When `TRUE` (default), a `NAME`
#'   column is included in the result.
#' @param include_station_location Logical. When `TRUE`, `LATITUDE`,
#'   `LONGITUDE`, and `ELEVATION` columns are added. Default `FALSE`.
#'
#' @return A data frame with one row per observation. The `STATION` column
#'   identifies the source station and `DATE` records the observation
#'   time. Additional columns depend on the requested `dataset` and
#'   `data_types`.
#'
#' @details
#' This function calls
#' `https://www.ncei.noaa.gov/access/services/data/v1`. Data are returned
#' in CSV format and read into a data frame with [readr::read_csv()].
#'
#' The `"daily-summaries"` dataset returns one row per station per day
#' with columns such as `PRCP`, `TMAX`, and `TMIN`. The `"global-hourly"`
#' dataset returns ISD records in which several key variables (`TMP`,
#' `WND`, `DEW`, `SLP`) contain comma-separated sub-fields rather than
#' simple numeric values.
#'
#' @seealso [ncei_stations()] to search for station identifiers,
#'   [ncei_datasets()] to list available datasets.
#'
#' @examples
#' \dontrun{
#' # Daily temperature and precipitation at a single station
#' ncei_data(
#'   dataset = "daily-summaries",
#'   stations = "USW00023183",
#'   start_date = "2023-01-01",
#'   end_date = "2023-12-31",
#'   data_types = c("TMAX", "TMIN", "PRCP")
#' )
#'
#' # Hourly ISD observations
#' ncei_data(
#'   dataset = "global-hourly",
#'   stations = "72469023183",
#'   start_date = "2023-06-01",
#'   end_date = "2023-06-30"
#' )
#' }
#'
#' @export
ncei_data <- function(
  dataset,
  stations,
  start_date,
  end_date,
  data_types = NULL,
  units = "metric",
  include_station_name = TRUE,
  include_station_location = FALSE
) {
  if (
    !is.character(dataset) ||
      length(dataset) != 1 ||
      is.na(dataset) ||
      !nzchar(dataset)
  ) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }
  if (
    !is.character(stations) ||
      length(stations) == 0 ||
      anyNA(stations) ||
      any(!nzchar(stations))
  ) {
    cli::cli_abort("{.arg stations} must be a non-empty character vector.")
  }
  start_date <- ncei_check_date(start_date)
  end_date <- ncei_check_date(end_date)
  if (!is.null(data_types)) {
    if (
      !is.character(data_types) ||
        length(data_types) == 0 ||
        anyNA(data_types)
    ) {
      cli::cli_abort("{.arg data_types} must be a character vector or `NULL`.")
    }
  }
  units <- match.arg(units, c("metric", "standard"))
  if (
    !is.logical(include_station_name) ||
      length(include_station_name) != 1 ||
      is.na(include_station_name)
  ) {
    cli::cli_abort("{.arg include_station_name} must be `TRUE` or `FALSE`.")
  }
  if (
    !is.logical(include_station_location) ||
      length(include_station_location) != 1 ||
      is.na(include_station_location)
  ) {
    cli::cli_abort(
      "{.arg include_station_location} must be `TRUE` or `FALSE`."
    )
  }

  req <- ncei_request(ncei_data_url) |>
    httr2::req_url_query(
      dataset = dataset,
      stations = paste(stations, collapse = ","),
      startDate = start_date,
      endDate = end_date,
      format = "csv",
      units = units,
      includeStationName = if (include_station_name) "true" else "false",
      includeStationLocation = if (include_station_location) "1" else "0"
    )

  if (!is.null(data_types)) {
    req <- req |>
      httr2::req_url_query(dataTypes = paste(data_types, collapse = ","))
  }

  tryCatch(
    {
      resp <- req |> http_req_perform()
      body <- httr2::resp_body_string(resp)
      readr::read_csv(I(body), show_col_types = FALSE, progress = FALSE)
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to retrieve data from the NCEI Data Service API.",
        parent = e
      )
    }
  )
}

#' List available NCEI datasets
#'
#' Retrieves metadata about a dataset from the NCEI Support Service API,
#' including available data types, spatial coverage, and temporal range.
#'
#' @param dataset Character string. The dataset identifier, such as
#'   `"daily-summaries"` or `"global-hourly"`.
#'
#' @return A list of dataset metadata as returned by the Support Service
#'   API. The structure varies by dataset but typically includes fields
#'   such as `id`, `name`, `dataTypes`, and `extent`.
#'
#' @examples
#' \dontrun{
#' ncei_datasets("daily-summaries")
#' ncei_datasets("global-hourly")
#' }
#'
#' @export
ncei_datasets <- function(dataset) {
  if (
    !is.character(dataset) ||
      length(dataset) != 1 ||
      is.na(dataset) ||
      !nzchar(dataset)
  ) {
    cli::cli_abort("{.arg dataset} must be a single non-empty string.")
  }

  url <- paste0(ncei_support_url, "/", dataset, ".json")

  tryCatch(
    {
      resp <- ncei_request(url) |> http_req_perform()
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to retrieve dataset metadata from the NCEI Support Service API.",
        parent = e
      )
    }
  )
}
