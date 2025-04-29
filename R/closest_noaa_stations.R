#' Find NOAA Stations Within Specified Radius
#'
#' @description
#' Identifies NOAA weather stations within a specified radius of a given location
#' using geodesic distance calculations. Returns station metadata including
#' distance from the target location.
#'
#' @param lat Numeric latitude of the target location in decimal degrees
#' @param long Numeric longitude of the target location in decimal degrees
#' @param dist_km Numeric search radius in kilometers
#' @param state Optional two-letter state code to filter stations
#' @param clean Logical indicating whether to return cleaned data (default: TRUE)
#'
#' @return A data frame of NOAA stations within the specified radius, including:
#' \itemize{
#'   \item Station identifiers (GHCND_ID, WBAN_ID, etc.)
#'   \item Station names and locations
#'   \item Elevation data
#'   \item Operational dates
#'   \item Distance from target location (in km)
#' }
#' Returns NULL if no stations are found within the radius.
#'
#' @details
#' Distance calculations use \code{\link[geosphere]{distGeo}} for highly accurate
#' geodesic distance calculations. See the function documentation for details
#' about the ellipsoid model and algorithm used.
#'
#' @examples
#' \dontrun{
#' # Find stations within 50 km of Konza Prairie Biological Station
#' kbps_stations <- closest_noaa_stations(39.1068806,-96.6117151, 50)
#'
#' # Find stations within 100 km of Lawrence, Kansas only
#' lfw_stations <- closest_noaa_stations(38.9717, -95.2353, 100, state = "KS")
#' #' }
#'
#' @importFrom geosphere distGeo
#' @importFrom stats complete.cases
#' @export
closest_noaa_stations <- function(lat, long, dist_km, state = NULL, clean = TRUE) {
  # Validate coordinates
  if (!is.numeric(lat) || !is.numeric(long) || !is.numeric(dist_km)) {
    stop("Latitude, longitude, and distance must be numeric values")
  }
  if (abs(lat) > 90 || abs(long) > 180) {
    stop("Invalid coordinates: latitude must be [-90,90] and longitude [-180,180]")
  }
  if (dist_km <= 0) {
    stop("Search distance must be positive")
  }

  # Load station data
  stations <- get_noaa_stations(state = state, clean = clean, debug = debug)

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
    p1 = matrix(c(long, lat), ncol = 2),
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
