#' Get current Iowa Environmental Mesonet observations
#'
#' Retrieves current observations from the Iowa Environmental Mesonet (IEM).
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param network Optional single IEM network identifier, such as `"IA_ASOS"`.
#' @param networkclass Optional single network class, such as `"ASOS"` or
#'   `"COOP"`.
#' @param wfo Optional single National Weather Service forecast office
#'   identifier, such as `"DMX"`.
#' @param country Optional single country identifier, such as `"US"`.
#' @param state Optional single state identifier, such as `"IA"`.
#' @param stations Optional character vector of station identifiers.
#' @param minutes Optional whole number giving the maximum observation age in
#'   minutes.
#'
#' @return A tibble containing current station metadata and observations
#'   returned by IEM, with station identifiers in `station_id`. Units and
#'   available variables vary by network.
#'
#' @details
#' At least one filter must be supplied. Station identifiers may be shared
#' across networks, so a station-only request can return multiple rows for a
#' single identifier. Use `network` with `stations` when you need one network.
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
#' current <- iem_current(network = "IA_ASOS")
#' dsm <- iem_current(network = "IA_ASOS", stations = "DSM")
#' }
#'
#' @export
iem_current <- function(
  network = NULL,
  networkclass = NULL,
  wfo = NULL,
  country = NULL,
  state = NULL,
  stations = NULL,
  minutes = NULL
) {
  if (!is.null(network)) {
    check_string(network, allow_empty = FALSE)
  }
  if (!is.null(networkclass)) {
    check_string(networkclass, allow_empty = FALSE)
  }
  if (!is.null(wfo)) {
    check_string(wfo, allow_empty = FALSE)
  }
  if (!is.null(country)) {
    check_string(country, allow_empty = FALSE)
  }
  if (!is.null(state)) {
    check_string(state, allow_empty = FALSE)
  }
  if (!is.null(stations)) {
    check_character(stations, allow_empty = FALSE, allow_na = FALSE)
  }
  if (!is.null(minutes)) {
    iem_check_whole_number(minutes)
  }

  has_filter <- any(
    !vapply(
      list(network, networkclass, wfo, country, state, stations),
      is.null,
      logical(1)
    )
  )
  if (!has_filter) {
    cli::cli_abort(
      paste0(
        "At least one of {.arg network}, {.arg networkclass}, {.arg wfo}, ",
        "{.arg country}, {.arg state}, or {.arg stations} must be supplied."
      )
    )
  }

  request <- iem_request("currents.json")
  if (!is.null(network)) {
    request <- httr2::req_url_query(request, network = network)
  }
  if (!is.null(networkclass)) {
    request <- httr2::req_url_query(request, networkclass = networkclass)
  }
  if (!is.null(wfo)) {
    request <- httr2::req_url_query(request, wfo = wfo)
  }
  if (!is.null(country)) {
    request <- httr2::req_url_query(request, country = country)
  }
  if (!is.null(state)) {
    request <- httr2::req_url_query(request, state = state)
  }
  if (!is.null(stations)) {
    request <- httr2::req_url_query(
      request,
      station = stations,
      .multi = "explode"
    )
  }
  if (!is.null(minutes)) {
    request <- httr2::req_url_query(request, minutes = minutes)
  }

  request |>
    iem_perform_json("Failed to fetch current IEM observations.") |>
    iem_as_tibble() |>
    iem_parse_time_columns() |>
    mesonet_rename_columns(c(station = "station_id", name = "station_name"))
}
