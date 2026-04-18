#' Check if an object has units
#'
#' This function determines whether an object has units attached to it,
#' typically from the \code{units} package. It provides a safe way to test
#' for the presence of units before performing unit-specific operations.
#'
#' @param x An object to test for units. Can be any R object, but typically
#'   a numeric vector, matrix, or array.
#'
#' @return A logical value: \code{TRUE} if the object has units attached,
#'   \code{FALSE} otherwise.
#'
#' @details
#' This function uses \code{\link[base]{inherits}} to check if the object
#' inherits from the "units" class. This is safer than checking the class
#' attribute directly, as it handles inheritance properly and avoids issues
#' with objects that have \code{NULL} class attributes.
#'
#' @examples
#' # Regular numeric vector (no units)
#' x <- c(1, 2, 3)
#' has_units(x)  # FALSE
#'
#' # With units (requires units package)
#' \dontrun{
#' library(units)
#' y <- set_units(c(1, 2, 3), "m")
#' has_units(y)  # TRUE
#'
#' z <- set_units(5, "kg")
#' has_units(z)  # TRUE
#' }
#'
#' # Other object types
#' has_units("text")     # FALSE
#' has_units(NULL)       # FALSE
#' has_units(list(1,2))  # FALSE
#'
#' @seealso
#' \code{\link[units]{set_units}}, \code{\link[units]{drop_units}},
#' \code{\link[base]{inherits}}
#'
#' @keywords internal
#' @export
has_units <- function(x) {
  inherits(x, "units")
}
#' Validate input data structure
#' @param df Input dataframe
#' @param metadata Metadata dataframe
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_input_data <- function(df, metadata) {
  # Check if dataframes are empty
  if (nrow(df) == 0) {
    stop("Input dataframe is empty")
  }
  if (nrow(metadata) == 0) {
    stop("Metadata dataframe is empty")
  }

  # Check if required columns exist in df
  if (!"Site" %in% names(df)) {
    stop("Input dataframe must contain a 'Site' column")
  }
  if (!"dateTime" %in% names(df)) {
    stop("Input dataframe must contain a 'dateTime' column")
  }

  # Check if required columns exist in metadata
  required_meta_cols <- c("Site", "Lat", "Long", "Elev_m")
  missing_meta_cols <- setdiff(required_meta_cols, names(metadata))
  if (length(missing_meta_cols) > 0) {
    stop(
      "Missing required metadata columns: ",
      paste(missing_meta_cols, collapse = ", ")
    )
  }

  # Check data types
  if (!inherits(df$dateTime, c("POSIXct", "POSIXt"))) {
    stop("dateTime column must be POSIXct")
  }

  # Validate metadata values
  if (!all(metadata$Lat >= -90 & metadata$Lat <= 90)) {
    stop("Invalid latitude values in metadata")
  }
  if (!all(metadata$Long >= -180 & metadata$Long <= 180)) {
    stop("Invalid longitude values in metadata")
  }

  # Check if all sites in df exist in metadata
  missing_sites <- setdiff(unique(df$Site), metadata$Site)
  if (length(missing_sites) > 0) {
    stop(
      "The following sites are missing from metadata: ",
      paste(missing_sites, collapse = ", ")
    )
  }

  TRUE
}

#' Validate imputation parameters
#' @param method Imputation method
#' @param maxgap Maximum gap size
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_parameters <- function(method, maxgap) {
  # Validate method
  valid_methods <- c("kalman", "ma", "linear", "spline")
  if (!method %in% valid_methods) {
    stop(
      "Invalid imputation method. Must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # Validate maxgap
  if (!is.numeric(maxgap) || maxgap < 0) {
    stop("maxgap must be a non-negative number")
  }

  TRUE
}

#' Split dataframe by site
#' @param df Dataframe to split
#' @return List of dataframes split by site
#' @keywords internal
#' @importFrom rlang .data
split_by_site <- function(df) {
  # Validate required columns
  required_columns <- c("Site")

  if (!all(required_columns %in% colnames(df))) {
    stop(
      "Missing required columns in metadata: ",
      paste(setdiff(required_columns, colnames(df)), collapse = ", ")
    )
  }
  df |>
    dplyr::group_by(.data$Site) |>
    dplyr::group_split()
}

#' Get site-specific metadata
#' @param site_name Name of the site
#' @param metadata Full metadata dataframe
#' @return Site-specific metadata
#' @keywords internal
#' @importFrom rlang .data
get_site_metadata <- function(site_name, metadata) {
  # Validate required columns
  required_columns <- c("Site", "Lat", "Long")

  if (!all(required_columns %in% colnames(metadata))) {
    stop(
      "Missing required columns in metadata: ",
      paste(setdiff(
        required_columns, colnames(metadata)
      ), collapse = ", ")
    )
  }

  # Filter metadata for the specific site
  site_meta <- metadata |>
    dplyr::filter(.data$Site == site_name)

  # Validate site presence
  if (nrow(site_meta) == 0) {
    stop(sprintf("Site '%s' not found in metadata", site_name))
  }

  # Validate latitude and longitude
  if (any(is.na(site_meta[c("Lat", "Long")]))) {
    stop(sprintf("Latitude or Longitude missing for site: %s", site_name))
  }

  site_meta
}


#' Convert Barometric Pressure to Atmospheres
#'
#' This helper function converts barometric pressure from various units
#' to atmospheres (\[atm\]). It is intended for internal use within the package.
#'
#' @param pressure Numeric. Barometric pressure value(s) to be converted.
#' @param units Character. Units of the input barometric pressure. Accepted values are
#'   `"atm"`, `"hPa"`, `"mbar"`, `"kPa"`, `"Torr"`, `"psi"`, and `"bar"`.
#'
#' @return Numeric. Barometric pressure in atmospheres (\[atm\]).
#'
#' @keywords internal
#' @export
convert_pressure_to_atm <- function(pressure, units) {
  if (units == "atm") {
    pressure_atm <- pressure
  } else if (units == "hPa" || units == "mbar") {
    pressure_atm <- pressure * 0.00098692316931427
  } else if (units == "kPa") {
    pressure_atm <- pressure * 0.0098692316931427
  } else if (units == "Torr") {
    pressure_atm <- pressure / 760
  } else if (units == "psi") {
    pressure_atm <- pressure * 0.0680459639
  } else if (units == "bar") {
    pressure_atm <- pressure * 0.98692316931427
  } else {
    stop("Please report barometric pressure in units of `atm`, `hPa`, `mbar`, `kPa`, `Torr`, `psi`, or `bar`.")
  }

  return(pressure_atm)
}


#' Get the Last Modified Time of a Remote File (Internal)
#'
#' This internal function retrieves the "Last-Modified" timestamp of a remote file by sending an HTTP HEAD request to the given URL. The result is memoised to cache results for repeated calls with the same URL.
#'
#' @param url A character string specifying the URL of the remote file.
#'
#' @return A POSIXlt object representing the last modified timestamp of the remote file, or \code{NULL} if the information is unavailable or an error occurs.
#'
#' @details The function sends an HTTP HEAD request to the specified URL using the \pkg{httr} package. If the server responds with a 200 status code and includes a "Last-Modified" header, the timestamp is parsed and returned. If the request fails or the "Last-Modified" header is missing, \code{NULL} is returned.
#'
#' @note The function uses \pkg{memoise} to cache results, so repeated calls with the same URL will not trigger additional HTTP requests.
#'
#' @import httr
#' @importFrom memoise memoise
#' @keywords internal
get_remote_mtime <- memoise::memoise(function(url) {
  tryCatch({
    headers <- httr::HEAD(url)
    if (httr::status_code(headers) == 200) {
      file_date <- httr::headers(headers)[["last-modified"]]
      file_date <- strptime(file_date, "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      return(file_date)
    }
    NULL
  }, error = function(e) NULL)
})
