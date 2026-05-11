#' Get Iowa Environmental Mesonet daily summaries
#'
#' Retrieves daily summary observations from the Iowa Environmental Mesonet
#' (IEM).
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param network A single IEM network identifier, such as `"IA_ASOS"`.
#' @param station Optional single station identifier, such as `"DSM"`.
#' @param date Optional single local calendar date as a `Date` object or
#'   `YYYY-MM-DD` string.
#' @param year Optional whole number year.
#' @param month Optional whole number month from 1 to 12. Requires `year`.
#'
#' @return A tibble containing daily summary observations returned by IEM, with
#'   station identifiers in `station_id` when returned. Units and available
#'   variables vary by network.
#'
#' @details
#' Use either `date` for all stations in a network on one local date, or `year`
#' and optional `month` for a longer station or network request. Do not supply
#' `date` together with `year` or `month`.
#'
#' @references
#' Iowa Environmental Mesonet API:
#' \url{https://mesonet.agron.iastate.edu/api/}
#'
#' IEM API v1 documentation:
#' \url{https://mesonet.agron.iastate.edu/api/1/docs}
#'
#' @examples
#' \dontrun{
#' daily <- iem_daily("IA_ASOS", station = "DSM", year = 2024)
#' network_day <- iem_daily("IA_ASOS", date = "2024-06-01")
#' }
#'
#' @export
iem_daily <- function(
  network,
  station = NULL,
  date = NULL,
  year = NULL,
  month = NULL
) {
  check_string(network, allow_empty = FALSE)
  if (!is.null(station)) {
    check_string(station, allow_empty = FALSE)
  }
  if (!is.null(date)) {
    date <- iem_check_date(date)
  }
  if (!is.null(year)) {
    iem_check_whole_number(year, min = 1)
  }
  if (!is.null(month)) {
    iem_check_whole_number(month, min = 1, max = 12)
  }

  if (is.null(date) && is.null(year)) {
    cli::cli_abort(
      "One of {.arg date} or {.arg year} must be supplied."
    )
  }
  if (!is.null(date) && (!is.null(year) || !is.null(month))) {
    cli::cli_abort(
      "{.arg date} cannot be supplied with {.arg year} or {.arg month}."
    )
  }
  if (!is.null(month) && is.null(year)) {
    cli::cli_abort("{.arg month} requires {.arg year}.")
  }

  request <- iem_request("daily.json") |>
    httr2::req_url_query(network = network)
  if (!is.null(station)) {
    request <- httr2::req_url_query(request, station = station)
  }
  if (!is.null(date)) {
    request <- httr2::req_url_query(request, date = date)
  }
  if (!is.null(year)) {
    request <- httr2::req_url_query(request, year = year)
  }
  if (!is.null(month)) {
    request <- httr2::req_url_query(request, month = month)
  }

  request |>
    iem_perform_json("Failed to fetch IEM daily summaries.") |>
    iem_as_tibble() |>
    iem_parse_time_columns() |>
    mesonet_rename_columns(c(station = "station_id", name = "station_name"))
}
