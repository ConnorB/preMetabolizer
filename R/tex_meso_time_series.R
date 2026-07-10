#' Get recent TexMesonet time-series data
#'
#' Retrieves recent time-series observations for a single TWDB TexMesonet
#' station.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param site_id TexMesonet station ID.
#' @param prior_minutes Number of minutes before the current time to retrieve.
#' @param variable Data type to retrieve. Use `"all"` for the charting fields
#'   endpoint, or one of `"temperature"`, `"humidity"`,
#'   `"barometric_pressure"`, `"precip"`, or `"wind_speed"` for the
#'   single-variable endpoints.
#'
#' @return A tibble of observations with UTC `date_time` values.
#'   Single-variable requests return `value` and `date_time` columns and have
#'   `"field_name"`, `"station_name"`, `"station_id"`, and `"units"`
#'   attributes. Requests with `variable = "all"` return one column per
#'   charting field and have `"station_name"` and `"station_id"` attributes.
#'
#' @details
#' TexMesonet's public time-series API retrieves recent observations by station
#' and look-back window. The API may return different fields by station because
#' not all stations measure every parameter.
#'
#' @references
#' TexMesonet APIs:
#' \url{https://www.texmesonet.org/Apis}
#'
#' @examples
#' \dontrun{
#' tex_meso_time_series(2, prior_minutes = 60)
#' tex_meso_time_series(2, prior_minutes = 60, variable = "temperature")
#' }
#'
#' @export
tex_meso_time_series <- function(
  site_id,
  prior_minutes,
  variable = c(
    "all",
    "temperature",
    "humidity",
    "barometric_pressure",
    "precip",
    "wind_speed"
  )
) {
  tex_meso_check_positive_int(site_id)
  tex_meso_check_positive_int(prior_minutes)
  variable <- rlang::arg_match(variable)

  endpoint <- switch(
    variable,
    all = "AllChartingFieldsById",
    temperature = "TemperatureById",
    humidity = "HumidityById",
    barometric_pressure = "BarometricPressureById",
    precip = "PrecipById",
    wind_speed = "WindSpeedById"
  )

  response <- tex_meso_request(endpoint) |>
    httr2::req_url_path_append(site_id, prior_minutes) |>
    tex_meso_perform_json(
      "Failed to fetch TexMesonet time-series data."
    )

  values <- tex_meso_as_tibble(response$values, numeric_exclude = "dateTime")
  if ("date_time" %in% names(values)) {
    values$date_time <- tex_meso_parse_datetime(values$date_time)
  }

  attr(values, "station_id") <- response$twdbStationId
  attr(values, "station_name") <- response$stationName
  if (!is.null(response$fieldName)) {
    attr(values, "field_name") <- response$fieldName
  }
  if (!is.null(response$units)) {
    attr(values, "units") <- response$units
  }

  values
}

#' Get recent TexMesonet time-series data (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [tex_meso_time_series()] instead.
#'
#' @examples
#' # Old:
#' # tex_meso_timeseries(2, prior_minutes = 60)
#' # New:
#' tex_meso_time_series(2, prior_minutes = 60)
#'
#' @keywords internal
#' @export
tex_meso_timeseries <- function(
  site_id,
  prior_minutes,
  variable = c(
    "all",
    "temperature",
    "humidity",
    "barometric_pressure",
    "precip",
    "wind_speed"
  )
) {
  lifecycle::deprecate_soft(
    "0.0.0.9000",
    "tex_meso_timeseries()",
    "tex_meso_time_series()"
  )
  tex_meso_time_series(
    site_id = site_id,
    prior_minutes = prior_minutes,
    variable = variable
  )
}
