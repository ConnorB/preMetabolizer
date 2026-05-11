#' Get one day of Iowa Environmental Mesonet observations
#'
#' Retrieves one local calendar day of observations for one IEM station.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param station A single station identifier, such as `"DSM"`.
#' @param network A single IEM network identifier, such as `"IA_ASOS"`.
#' @param date A single local calendar date as a `Date` object or `YYYY-MM-DD`
#'   string. Defaults to `Sys.Date()`.
#' @param full Logical. If `TRUE`, request all available observation fields.
#'
#' @return A tibble containing one day of station observations returned by IEM,
#'   with station identifiers in `station_id` when returned. Units and
#'   available variables vary by network.
#'
#' @details
#' The `date` argument is interpreted by IEM as a local station calendar date.
#' UTC timestamps are parsed as `POSIXct` values in the UTC time zone. Local
#' timestamp fields are returned as character values because station time zones
#' vary by network.
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
#' obs <- iem_obhistory("DSM", network = "IA_ASOS", date = "2024-06-01")
#' }
#'
#' @export
iem_obhistory <- function(station, network, date = Sys.Date(), full = FALSE) {
  check_string(station, allow_empty = FALSE)
  check_string(network, allow_empty = FALSE)
  check_bool(full)
  date <- iem_check_date(date)

  iem_request("obhistory.json") |>
    httr2::req_url_query(
      network = network,
      station = station,
      date = date,
      full = tolower(as.character(full))
    ) |>
    iem_perform_json("Failed to fetch IEM observation history.") |>
    iem_as_tibble() |>
    iem_parse_time_columns() |>
    mesonet_rename_columns(c(station = "station_id", name = "station_name"))
}
