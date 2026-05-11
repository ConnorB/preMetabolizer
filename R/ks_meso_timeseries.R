#' Get Kansas Mesonet time-series data
#'
#' Retrieves weather observations for one or more Kansas Mesonet stations or a
#' supported Kansas Mesonet subnetwork.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param stations Character vector of station names to retrieve data for. Use
#'   `"all"` to retrieve data for all stations. Must be `NULL` when `network`
#'   is supplied.
#' @param network Network name to retrieve data for, one of:
#'   - `"BBW"`: Big Bend Groundwater Management District
#'   - `"EBW"`: Equus Beds Groundwater Management District
#'   - `"KSRE"`: K-State Research and Extension
#'
#'   This is an alternative to `stations`.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` or
#'   `YYYYMMDD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD`
#'   format.
#' @param interval Data interval. Must be one of `"hour"`, `"5min"`, or
#'   `"day"`.
#' @param vars Character vector of variables to retrieve. Defaults to common
#'   weather variables.
#'
#' @return A tibble containing requested observations with snake_case column
#'   names. Kansas Mesonet timestamps are parsed as `POSIXct` values in the
#'   fixed Mesonet time zone, `Etc/GMT+6`.
#'
#' @details
#' Large date ranges are split into chunks automatically to stay within the
#' Kansas Mesonet API record limit. Data are returned directly and are not
#' written to a local cache.
#'
#' Kansas Mesonet data are preliminary and subject to revision. Cite the Kansas
#' Mesonet when sharing, publishing, or otherwise disseminating data accessed
#' with this function. A suggested citation format is: Kansas Mesonet, year:
#' webpage title. Accessed date, webpage URL. Review the Kansas Mesonet data
#' usage policy before automated use; automated page scraping or data ingesting
#' without written consent is not permitted.
#'
#' @references
#' Kansas Mesonet data usage policy:
#' \url{https://mesonet.k-state.edu/about/usage/}
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' konza <- ks_meso_timeseries(
#'   stations = "Konza Prairie",
#'   start_date = "2024-06-01",
#'   end_date = "2024-06-07",
#'   interval = "hour",
#'   vars = c("TEMP2MAVG", "RELHUM2MAVG", "PRESSUREAVG")
#' )
#' }
#'
#' @export
ks_meso_timeseries <- function(
  stations = NULL,
  network = NULL,
  start_date,
  end_date,
  interval,
  vars = NULL
) {
  interval <- rlang::arg_match(interval, c("hour", "5min", "day"))
  if (!is.null(stations) && !is.null(network)) {
    cli::cli_abort("Use only one of {.arg stations} and {.arg network}.")
  }
  if (is.null(stations) && is.null(network)) {
    cli::cli_abort("One of {.arg stations} or {.arg network} must be supplied.")
  }
  if (
    !is.null(stations) && (!is.character(stations) || length(stations) == 0)
  ) {
    cli::cli_abort("{.arg stations} must be a character vector.")
  }
  if (!is.null(network)) {
    network <- rlang::arg_match(network, c("KSRE", "BBW", "EBW"))
  }

  dates <- validate_ks_meso_dates(start_date, end_date)
  vars <- if (is.null(vars)) {
    c(
      "PRESSUREAVG",
      "TEMP2MAVG",
      "TEMP2MMIN",
      "TEMP2MMAX",
      "RELHUM2MAVG",
      "PRECIP"
    )
  } else {
    vars
  }

  stations_metadata <- ks_meso_stations() |>
    dplyr::select(
      "station_name",
      "station_id",
      "network",
      "network_name"
    )

  if (!is.null(stations)) {
    available_stations <- stations_metadata$station_name
    stations_to_check <- setdiff(stations, "all")
    invalid_stations <- setdiff(stations_to_check, available_stations)
    if (length(invalid_stations) > 0) {
      cli::cli_abort("Invalid station{?s}: {.val {invalid_stations}}.")
    }
  }

  available_vars <- ks_meso_vars()$csv_heading
  invalid_vars <- setdiff(vars, available_vars)
  if (length(invalid_vars) > 0) {
    cli::cli_abort("Invalid variable{?s}: {.val {invalid_vars}}.")
  }

  records_per_interval <- list(
    hour = 24,
    "5min" = 288,
    day = 1
  )[[interval]]
  max_days_per_call <- floor((3000 * 0.9) / records_per_interval)
  if (interval == "5min") {
    max_days_per_call <- min(max_days_per_call, 10)
  }

  request_names <- if (is.null(stations)) network else stations
  query_type <- if (is.null(network)) "station" else "network"
  date_sequence <- seq(dates$start, dates$end, by = "days")
  chunks <- split(
    date_sequence,
    ceiling(seq_along(date_sequence) / max_days_per_call)
  )

  request_data <- ks_meso_build_timeseries_requests(
    request_names,
    query_type,
    chunks,
    interval,
    vars
  )
  responses <- http_req_perform_parallel(
    request_data$requests,
    on_error = "continue"
  )

  data <- vector("list", length(responses))
  for (i in seq_along(responses)) {
    response <- responses[[i]]
    info <- request_data$metadata[[i]]

    if (inherits(response, "error")) {
      cli::cli_abort(
        c(
          "Failed to fetch Kansas Mesonet data.",
          "i" = "Request for {info$query_type} {.val {info$request_name}} from {info$start_time} to {info$end_time} failed.",
          "x" = conditionMessage(response)
        )
      )
    }

    data[[i]] <- ks_meso_read_csv(
      I(httr2::resp_body_string(response)),
      na = c("", "NA", "M")
    )

    if (!"station_name" %in% names(data[[i]])) {
      data[[i]]$station_name <- if (identical(info$query_type, "station")) {
        info$request_name
      } else {
        NA_character_
      }
    }
  }

  dplyr::bind_rows(data) |>
    dplyr::left_join(stations_metadata, by = "station_name") |>
    dplyr::select(
      dplyr::any_of(c(
        "station_name",
        "station_id",
        "network",
        "network_name",
        "timestamp"
      )),
      dplyr::everything()
    )
}
