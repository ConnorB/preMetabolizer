#' Find NOAA stations near a location
#'
#' @description
#' Identifies NOAA GHCND weather stations within a specified radius of a
#' target location. Station candidates are retrieved via the NCEI Search
#' API using a bounding box and then filtered to the exact circular
#' radius using geodesic distance.
#'
#' @param latitude,longitude Numeric. Target coordinates in decimal
#'   degrees. Latitude must be between -90 and 90; longitude must be
#'   between -180 and 180. Western longitudes are negative.
#' @param dist_km Numeric. Search radius in kilometres.
#' @param start_date,end_date Optional date range. When supplied only
#'   stations whose period of record overlaps this range are returned.
#'   Accepts `Date` objects or `"YYYY-MM-DD"` strings.
#' @param data_types Optional character vector of GHCND data type codes
#'   (e.g., `c("TMAX", "TMIN")`). When supplied only stations carrying
#'   all requested types are returned.
#' @param lat,long,lon `r lifecycle::badge("deprecated")` Use `latitude`
#'   and `longitude` instead.
#'
#' @return A [tibble][tibble::tibble-package] of NOAA stations within
#'   `dist_km`, sorted by ascending distance. The first column is
#'   `distance_km`; remaining columns are those returned by
#'   [get_noaa_stations()]. Returns `NULL` when no stations are found
#'   within the requested radius.
#'
#' @details
#' Distances are calculated with [geosphere::distGeo()] using the
#' default WGS84 ellipsoid.
#'
#' The search first queries the NCEI Search API using a square bounding
#' box of side `2 * dist_km` centred on the target point, then trims the
#' result to the circular radius. This requires one API call and avoids
#' downloading the entire station database.
#'
#' @seealso [get_noaa_stations()], [ncei_bbox()], [ncei_stations()]
#'
#' @examples
#' \dontrun{
#' # Stations within 50 km of Konza Prairie Biological Station
#' closest_noaa_stations(
#'   latitude = 39.1068806,
#'   longitude = -96.6117151,
#'   dist_km = 50
#' )
#'
#' # Restrict to stations with daily temperature data since 2000
#' closest_noaa_stations(
#'   latitude = 39.1068806,
#'   longitude = -96.6117151,
#'   dist_km = 100,
#'   data_types = c("TMAX", "TMIN"),
#'   start_date = "2000-01-01"
#' )
#' }
#'
#' @export
closest_noaa_stations <- function(
  latitude,
  longitude,
  dist_km,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  lat = lifecycle::deprecated(),
  long = lifecycle::deprecated(),
  lon = lifecycle::deprecated()
) {
  if (lifecycle::is_present(lat)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "closest_noaa_stations(lat)",
      "closest_noaa_stations(latitude)"
    )
    if (!missing(latitude)) {
      cli::cli_abort(
        "Use only one of {.arg latitude} and deprecated {.arg lat}."
      )
    }
    latitude <- lat
  }
  if (lifecycle::is_present(long)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "closest_noaa_stations(long)",
      "closest_noaa_stations(longitude)"
    )
    if (!missing(longitude)) {
      cli::cli_abort(
        "Use only one of {.arg longitude} and deprecated {.arg long}."
      )
    }
    longitude <- long
  }
  if (lifecycle::is_present(lon)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "closest_noaa_stations(lon)",
      "closest_noaa_stations(longitude)"
    )
    if (!missing(longitude)) {
      cli::cli_abort(
        "Use only one of {.arg longitude} and deprecated {.arg lon}."
      )
    }
    longitude <- lon
  }

  if (missing(latitude)) {
    cli::cli_abort("{.arg latitude} is required.")
  }
  if (missing(longitude)) {
    cli::cli_abort("{.arg longitude} is required.")
  }
  if (missing(dist_km)) {
    cli::cli_abort("{.arg dist_km} is required.")
  }

  if (
    !is.numeric(latitude) ||
      length(latitude) != 1 ||
      is.na(latitude) ||
      !is.finite(latitude)
  ) {
    cli::cli_abort("{.arg latitude} must be a single finite number.")
  }
  if (
    !is.numeric(longitude) ||
      length(longitude) != 1 ||
      is.na(longitude) ||
      !is.finite(longitude)
  ) {
    cli::cli_abort("{.arg longitude} must be a single finite number.")
  }
  if (
    !is.numeric(dist_km) ||
      length(dist_km) != 1 ||
      is.na(dist_km) ||
      !is.finite(dist_km)
  ) {
    cli::cli_abort("{.arg dist_km} must be a single finite number.")
  }
  if (abs(latitude) > 90) {
    cli::cli_abort("{.arg latitude} must be between -90 and 90.")
  }
  if (abs(longitude) > 180) {
    cli::cli_abort("{.arg longitude} must be between -180 and 180.")
  }
  if (dist_km <= 0) {
    cli::cli_abort("{.arg dist_km} must be greater than 0.")
  }

  bbox <- ncei_bbox(latitude, longitude, dist_km)

  stations <- get_noaa_stations(
    bbox = bbox,
    start_date = start_date,
    end_date = end_date,
    data_types = data_types
  )

  if (nrow(stations) == 0) {
    cli::cli_inform("No NOAA stations found within {dist_km} km.")
    return(NULL)
  }

  if (!all(c("latitude", "longitude") %in% names(stations))) {
    cli::cli_abort(
      "{.fn get_noaa_stations} must return {.field latitude} and {.field longitude} columns."
    )
  }

  valid_coords <- stats::complete.cases(stations[, c("latitude", "longitude")])
  if (!any(valid_coords)) {
    cli::cli_inform("No NOAA stations with valid coordinates found.")
    return(NULL)
  }
  stations <- stations[valid_coords, ]

  distances <- geosphere::distGeo(
    p1 = matrix(c(longitude, latitude), ncol = 2),
    p2 = matrix(c(stations$longitude, stations$latitude), ncol = 2)
  )

  in_radius <- distances <= dist_km * 1000
  if (!any(in_radius)) {
    cli::cli_inform("No NOAA stations found within {dist_km} km.")
    return(NULL)
  }

  result <- stations[in_radius, ]
  result$distance_km <- distances[in_radius] / 1000

  result <- result[, c("distance_km", setdiff(names(result), "distance_km"))]
  result <- result[order(result$distance_km), ]
  result <- result[, colSums(!is.na(result)) > 0]

  rownames(result) <- NULL
  result
}
