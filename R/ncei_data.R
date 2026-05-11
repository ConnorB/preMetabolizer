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
#' @param include_station_name Logical. When `TRUE` (default), a
#'   `station_name` column is included in the result.
#' @param include_station_location Logical. When `TRUE`, `latitude`,
#'   `longitude`, and `elevation` columns are added. Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble-package] with one row per observation
#'   and snake_case column names. Leading columns (when present) are
#'   `station_id`, `station_name`, `latitude`, `longitude`, `elevation`,
#'   and either `datetime` (`POSIXct` UTC, for sub-daily datasets) or
#'   `date` (`Date`, for daily and coarser datasets). Remaining columns
#'   depend on the requested `dataset` and `data_types`. Columns that are
#'   entirely `NA` are dropped.
#'
#' @details
#' This function calls
#' `https://www.ncei.noaa.gov/access/services/data/v1`. Data are returned
#' in CSV format and parsed into a tibble.
#'
#' For `dataset = "global-hourly"` (Integrated Surface Database), the
#' mandatory packed fields are expanded into typed numeric columns with
#' SI units and missing-value sentinels converted to `NA`:
#'
#' \describe{
#'   \item{`WND`}{`wind_direction` (degrees), `wind_direction_quality`,
#'     `wind_type_code`, `wind_speed` (m/s), `wind_speed_quality`.}
#'   \item{`CIG`}{`ceiling_height` (m), `ceiling_quality`,
#'     `ceiling_determination_code`, `ceiling_cavok`.}
#'   \item{`VIS`}{`visibility` (m), `visibility_quality`,
#'     `visibility_variability_code`, `visibility_variability_quality`.}
#'   \item{`TMP`}{`temperature` (°C), `temperature_quality`.}
#'   \item{`DEW`}{`dew_point_temperature` (°C), `dew_point_quality`.}
#'   \item{`SLP`}{`sea_level_pressure` (hPa),
#'     `sea_level_pressure_quality`.}
#'   \item{`AA1`–`AA4`}{`precipitation_period_hours` (hr),
#'     `precipitation` (mm), `precipitation_condition_code`,
#'     `precipitation_quality`.}
#' }
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
#' # Hourly ISD observations with expanded mandatory fields
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
  check_string(dataset, allow_empty = FALSE)
  check_character(stations, allow_empty = FALSE, allow_na = FALSE)
  if (any(!nzchar(stations))) {
    cli::cli_abort("{.arg stations} must not contain empty strings.")
  }
  start_date <- ncei_check_date(start_date)
  end_date <- ncei_check_date(end_date)
  check_character(
    data_types,
    allow_null = TRUE,
    allow_empty = FALSE,
    allow_na = FALSE
  )
  units <- rlang::arg_match(units, c("metric", "standard"))
  check_bool(include_station_name)
  check_bool(include_station_location)

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
      ncei_parse_data_csv(body, dataset)
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
  check_string(dataset, allow_empty = FALSE)

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
