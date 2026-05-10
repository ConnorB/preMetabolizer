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
#' @return A tibble of observations with UTC `dateTime` values. Single-variable
#'   requests return `value` and `dateTime` columns and have `"field_name"`,
#'   `"station_name"`, `"twdb_station_id"`, and `"units"` attributes. Requests
#'   with `variable = "all"` return one column per charting field and have
#'   `"station_name"` and `"twdb_station_id"` attributes.
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
#' tex_meso_timeseries(2, prior_minutes = 60)
#' tex_meso_timeseries(2, prior_minutes = 60, variable = "temperature")
#' }
#'
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
  if ("dateTime" %in% names(values)) {
    values$dateTime <- tex_meso_parse_datetime(values$dateTime)
  }

  attr(values, "twdb_station_id") <- response$twdbStationId
  attr(values, "station_name") <- response$stationName
  if (!is.null(response$fieldName)) {
    attr(values, "field_name") <- response$fieldName
  }
  if (!is.null(response$units)) {
    attr(values, "units") <- response$units
  }

  values
}
