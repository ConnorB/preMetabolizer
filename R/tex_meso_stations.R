#' Get TexMesonet station information
#'
#' Retrieves name, location, and status metadata for Texas Water Development
#' Board (TWDB) stations from the TexMesonet API.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param active Optional logical value used to filter stations by active
#'   status. If `NULL` (default), active and inactive stations are returned.
#' @param displayed Optional logical value used to filter stations by whether
#'   they are displayed by TexMesonet. If `NULL` (default), displayed and hidden
#'   stations are returned.
#'
#' @return A tibble containing station metadata returned by TexMesonet,
#'   including `station_id`, `station_name`, display ID, state, county,
#'   latitude, longitude, elevation in feet, active status, and online date.
#'
#' @details
#' TexMesonet is managed by the Texas Water Development Board. Its public API
#' provides near-real-time data for TWDB weather and soil observing stations.
#' Review the TexMesonet disclaimer before automated use.
#'
#' @references
#' TexMesonet APIs:
#' \url{https://www.texmesonet.org/Apis}
#'
#' TexMesonet disclaimer:
#' \url{https://www.texmesonet.org/About}
#'
#' @examples
#' \dontrun{
#' stations <- tex_meso_stations(active = TRUE)
#' subset(stations, county == "Blanco")
#' }
#'
#' @export
tex_meso_stations <- function(active = NULL, displayed = NULL) {
  if (!is.null(active)) {
    check_bool(active)
  }
  if (!is.null(displayed)) {
    check_bool(displayed)
  }

  stations <- tex_meso_request("Stations") |>
    tex_meso_perform_json(
      "Failed to fetch TexMesonet station metadata."
    ) |>
    tex_meso_as_tibble(
      names = c(
        stationId = "station_id",
        stationName = "station_name"
      )
    )

  if ("online_date" %in% names(stations)) {
    stations$online_date <- as.Date(stations$online_date, format = "%m/%d/%Y")
  }

  if (!is.null(active)) {
    stations <- stations[stations$active %in% active, , drop = FALSE]
  }
  if (!is.null(displayed)) {
    stations <- stations[
      stations$station_display %in% displayed,
      ,
      drop = FALSE
    ]
  }

  tibble::as_tibble(stations)
}
