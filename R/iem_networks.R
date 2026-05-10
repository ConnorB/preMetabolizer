#' Get Iowa Environmental Mesonet network identifiers
#'
#' Retrieves the network table from the Iowa Environmental Mesonet (IEM) API.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A tibble containing IEM network identifiers, network names, time
#'   zones, geographic extents, and windrose update timestamps when available.
#'
#' @details
#' The Iowa Environmental Mesonet groups stations into networks such as
#' `IA_ASOS`, `IA_COOP`, and other state, roadway, hydrological, and special
#' observing networks. Use [iem_networks()] to discover valid `network` values
#' for other IEM helpers.
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
#' networks <- iem_networks()
#' subset(networks, id == "IA_ASOS")
#' }
#'
#' @export
iem_networks <- function() {
  iem_request("networks.json") |>
    iem_perform_json("Failed to fetch IEM network metadata.") |>
    iem_as_tibble() |>
    iem_parse_time_columns()
}
