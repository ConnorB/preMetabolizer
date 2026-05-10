#' Get Iowa Environmental Mesonet station metadata
#'
#' Retrieves station metadata for one IEM network or one station identifier.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param network A single IEM network identifier, such as `"IA_ASOS"`.
#' @param station_id A single station identifier, such as `"DSM"`.
#'
#' @return A tibble containing station metadata returned by IEM, including
#'   station identifiers, names, network identifiers, time zones, archive dates,
#'   and longitude and latitude when available.
#'
#' @details
#' `iem_stations()` returns all stations in one network. `iem_station()` looks
#' up one station identifier across IEM networks, which is useful because some
#' identifiers are shared by more than one network.
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
#' stations <- iem_stations("IA_ASOS")
#' iem_station("DSM")
#' }
#'
#' @export
iem_stations <- function(network) {
  check_string(network, allow_empty = FALSE)

  iem_request("network") |>
    httr2::req_url_path_append(paste0(network, ".json")) |>
    iem_perform_json("Failed to fetch IEM station metadata.") |>
    iem_as_tibble() |>
    iem_parse_time_columns()
}

#' @rdname iem_stations
#' @export
iem_station <- function(station_id) {
  check_string(station_id, allow_empty = FALSE)

  iem_request("station") |>
    httr2::req_url_path_append(paste0(station_id, ".json")) |>
    iem_perform_json("Failed to fetch IEM station metadata.") |>
    iem_as_tibble() |>
    iem_parse_time_columns()
}
