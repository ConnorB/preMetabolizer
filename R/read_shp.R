#' Read Shapefile or Zipped Shapefile
#'
#' Reads a shapefile or shapefile contained in a zip archive, automatically handling
#' path specifications and layer detection. This is a convenience wrapper around
#' \code{\link[sf]{st_read}} that simplifies reading common shapefile formats.
#'
#' @param path Path to the shapefile (.shp), directory containing shapefile components,
#'   or zip file containing a shapefile. Tilde expansion is performed.
#' @param layer Layer name to read (optional). If not specified and path points to
#'   a .shp file, the layer name will be derived from the filename. For directories
#'   or zip files with multiple layers, the first layer will be used with a warning.
#'
#' @return An sf object containing the spatial data from the shapefile.
#'
#' @details
#' The function handles several input types:
#' \itemize{
#'   \item Direct paths to .shp files (automatically extracts directory and layer name)
#'   \item Directories containing shapefile components (auto-detects layer)
#'   \item Zip files containing shapefiles (uses GDAL's virtual file system)
#' }
#'
#' When reading from zip files, the function uses GDAL's \code{/vsizip/} virtual file
#' system prefix.
#'
#' @examples
#' \dontrun{
#' # Read from .shp file path
#' shp_data <- read_shp("path/to/file.shp")
#'
#' # Read from directory containing shapefile components
#' shp_data <- read_shp("path/to/shapefile_directory")
#'
#' # Read from zip file
#' shp_data <- read_shp("path/to/archive.zip")
#'
#' # Specify layer explicitly
#' shp_data <- read_shp("path/to/multi_layer.zip", layer = "specific_layer")
#' }
#'
#' @seealso \code{\link[sf]{st_read}} for the underlying reading function,
#'   \code{\link[sf]{st_layers}} for layer detection
#' @export
read_shp <- function(path, layer = NULL) {
  # Normalize path to expand ~ and get absolute path
  path <- normalizePath(path, mustWork = FALSE)
  
  # Check if path is a zip file
  is_zip <- grepl("\\.zip$", path, ignore.case = TRUE)
  
  if (is_zip) {
    dsn <- paste0("/vsizip/", path)
  } else {
    # If it's a .shp file, use its directory as dsn and extract layer name
    if (grepl("\\.shp$", path, ignore.case = TRUE)) {
      dsn <- dirname(path)
      if (is.null(layer)) {
        layer <- tools::file_path_sans_ext(basename(path))
      }
    } else {
      # Assume it's a directory containing shapefile components
      dsn <- path
    }
  }
  
  # Auto-detect layer if not specified
  if (is.null(layer)) {
    layers <- sf::st_layers(dsn)$name
    if (length(layers) == 0) {
      stop("No layers found at the specified path.")
    } else if (length(layers) > 1) {
      warning("Multiple layers found. Using the first one: ", layers[1])
    }
    layer <- layers[1]
  }
  
  sf::st_read(dsn = dsn, layer = layer, quiet = TRUE)
}
