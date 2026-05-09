#' Find NOAA stations near a location
#'
#' @description
#' Identifies NOAA weather stations within a specified radius of a target
#' location using geodesic distance calculations.
#'
#' @param latitude,longitude Numeric. Target coordinates in decimal degrees.
#'   Latitude must be between -90 and 90; longitude must be between -180 and
#'   180. Western longitudes are negative.
#' @param dist_km Numeric. Search radius in kilometers.
#' @param state Optional two-letter state code used to filter stations before
#'   calculating distances.
#' @param clean Logical. If `TRUE`, return cleaned station metadata from
#'   [get_noaa_stations()] and drop empty columns from the result.
#' @param lat,long,lon `r lifecycle::badge("deprecated")` Use `latitude` and
#'   `longitude` instead.
#'
#' @return A data frame of NOAA stations within `dist_km`, sorted by distance.
#'   The first column is `distance_km`. Returns `NULL` when no stations are
#'   found in the requested radius or when available station metadata does not
#'   contain usable coordinates.
#'
#' @details
#' Distances are calculated with [geosphere::distGeo()] using its default WGS84
#' ellipsoid.
#'
#' @examples
#' \dontrun{
#' # Find stations within 50 km of Konza Prairie Biological Station
#' closest_noaa_stations(
#'   latitude = 39.1068806,
#'   longitude = -96.6117151,
#'   dist_km = 50
#' )
#'
#' # Find stations within 100 km of Lawrence, Kansas only
#' closest_noaa_stations(
#'   latitude = 38.9717,
#'   longitude = -95.2353,
#'   dist_km = 100,
#'   state = "KS"
#' )
#' }
#'
#' @importFrom geosphere distGeo
#' @importFrom stats complete.cases
#' @importFrom cli cli_abort
#' @export
closest_noaa_stations <- function(
  latitude,
  longitude,
  dist_km,
  state = NULL,
  clean = TRUE,
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

  # Load station data
  stations <- get_noaa_stations(state = state, clean = clean)

  # Check for stations with valid coordinates
  valid_coords <- stats::complete.cases(stations[, c("LAT_DEC", "LON_DEC")])
  if (sum(valid_coords) == 0) {
    message("No stations with valid coordinates found")
    return(NULL)
  }
  stations <- stations[valid_coords, ]

  # Convert km to meters (for distGeo)
  dist_m <- dist_km * 1000

  # Calculate distances using distGeo (WGS84 by default)
  distances <- geosphere::distGeo(
    p1 = matrix(c(longitude, latitude), ncol = 2),
    p2 = matrix(c(stations$LON_DEC, stations$LAT_DEC), ncol = 2)
  )

  # Filter stations within radius
  in_radius <- distances <= dist_m
  if (sum(in_radius) == 0) {
    message("No stations found within ", dist_km, " km radius")
    return(NULL)
  }

  # Create output dataframe
  result <- stations[in_radius, ]
  result$distance_km <- distances[in_radius] / 1000

  # Reorder columns with distance first
  result <- result[, c("distance_km", setdiff(names(result), "distance_km"))]

  # Sort by distance
  result <- result[order(result$distance_km), ]

  # Remove empty columns if cleaned
  if (clean) {
    result <- result[, colSums(!is.na(result)) > 0]
  }

  rownames(result) <- NULL
  return(result)
}
