#' Initialize Package Cache Directories
#'
#' @description
#' Internal function executed automatically when the package is loaded. It initializes
#' cache directories using R's standardized user cache directory conventions.
#'
#' @param libname Library directory where the package is installed.
#' @param pkgname Name of the package.
#'
#' @keywords internal
#' @noRd
#' @importFrom tools R_user_dir
NULL  # This documents all internal functions
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  setup_cache <- function(cache_name) {
    cache_path <- tools::R_user_dir(pkgname, which = "cache") |>
      file.path(cache_name)

    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
    }

    cache_path
  }

  options(
    preMetabolizer.noaa_cache = setup_cache("noaa_cache"),
    preMetabolizer.mesonet_cache = setup_cache("mesonet_cache"),
    preMetabolizer.nasa_cache = setup_cache("nasa_cache")
  )
}

#' Retrieve NOAA Cache Path
#'
#' @return Path to NOAA cache directory.
#' @keywords internal
#' @noRd
noaa_cache <- function() {
  getOption("preMetabolizer.noaa_cache", stop("NOAA cache path not set."))
}

#' Retrieve Mesonet Cache Path
#'
#' @return Path to Mesonet cache directory.
#' @keywords internal
#' @noRd
mesonet_cache <- function() {
  getOption("preMetabolizer.mesonet_cache", stop("Mesonet cache path not set."))
}

#' Retrieve NASAPower Cache Path
#'
#' @return Path to NASA POWER cache directory.
#' @keywords internal
#' @noRd
nasa_cache <- function() {
  getOption("preMetabolizer.nasa_cache", stop("NASAPower cache path not set."))
}

