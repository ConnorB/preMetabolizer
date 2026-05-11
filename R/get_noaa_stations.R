#' Get NOAA station information
#'
#' Searches for NOAA weather stations in the NCEI daily-summaries
#' (GHCND) dataset using the NCEI Search Service API. This is the
#' preferred way to find station identifiers before downloading data
#' with [ncei_data()] or [get_ghcnh()].
#'
#' @param bbox Optional numeric vector `c(north, west, south, east)` in
#'   decimal degrees. When supplied only stations within this bounding
#'   box are returned. Use [ncei_bbox()] to build a bounding box from a
#'   centre point and radius. When `NULL` no geographic filter is applied.
#' @param start_date,end_date Optional date range filter. Accepts `Date`
#'   objects or `"YYYY-MM-DD"` strings. When supplied only stations whose
#'   period of record overlaps this range are returned.
#' @param data_types Optional character vector of GHCND data type codes
#'   (e.g., `c("TMAX", "TMIN")`). When supplied only stations carrying
#'   all requested types are returned.
#' @param text Optional character string. Filters stations by name.
#' @param limit Integer. Maximum number of stations to return (1–1000,
#'   default 1000).
#' @param offset Integer. Zero-based pagination offset (default 0).
#'
#' @return A [tibble][tibble::tibble-package] with columns `station_id`,
#'   `station_name`, `latitude`, `longitude`, `start_date`, and
#'   `end_date`. Returns an empty tibble when no stations match the
#'   query.
#'
#' @details
#' Station identifiers in `station_id` are bare GHCND IDs (e.g.,
#' `"USW00023183"`) stripped of their `"GHCND:"` prefix. Pass them
#' directly to [ncei_data()] or [get_ghcnh()].
#'
#' This function replaces the previous MSHR flat-file approach. Station
#' metadata is now retrieved on demand from the NCEI Search Service API
#' rather than downloaded as a large fixed-width archive.
#'
#' For distance-based searches use [closest_noaa_stations()], which
#' builds the bounding box from a latitude, longitude, and search radius.
#'
#' @seealso [closest_noaa_stations()], [ncei_bbox()], [ncei_stations()],
#'   [ncei_data()], [get_ghcnh()]
#'
#' @examples
#' \dontrun{
#' # All GHCND stations in a region
#' get_noaa_stations(bbox = c(40, -100, 38, -98))
#'
#' # Stations with at least temperature data since 2000
#' get_noaa_stations(
#'   data_types = c("TMAX", "TMIN"),
#'   start_date = "2000-01-01"
#' )
#' }
#'
#' @export
get_noaa_stations <- function(
  bbox = NULL,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  text = NULL,
  limit = 1000L,
  offset = 0L
) {
  ncei_stations(
    dataset = "daily-summaries",
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    data_types = data_types,
    text = text,
    limit = limit,
    offset = offset
  )
}
