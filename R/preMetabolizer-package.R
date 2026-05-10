#' preMetabolizer: Prepare data for stream metabolism modeling
#'
#' preMetabolizer provides helpers for building the environmental time series
#' commonly needed before fitting stream metabolism models. It includes tools
#' to download external meteorological and elevation data, align irregular time
#' series, convert hydrology and atmospheric units, calculate water and gas
#' chemistry quantities, and assemble light and pressure inputs for
#' streamMetabolizer.
#'
#' @section Coordinates:
#' Functions that require locations use `latitude` and `longitude` in decimal
#' degrees. Latitude ranges from -90 to 90, longitude ranges from -180 to 180,
#' and western longitudes are negative.
#'
#' @section Main workflows:
#' \itemize{
#'   \item Find and download meteorological data with [get_noaa_stations()],
#'     [closest_noaa_stations()], [download_ghcnh()], [get_nasa_data()], and
#'     [get_ks_meso()]. Retrieve mesonet station observations with
#'     [iem_networks()], [iem_current()], [iem_obhistory()],
#'     [tex_meso_stations()], [tex_meso_current()], and
#'     [tex_meso_timeseries()].
#'   \item Retrieve or calculate site context with [get_usgs_elev()],
#'     [correct_bp()], [convert_UTC_to_solartime()], and [calc_light()].
#'   \item Prepare model inputs with [even_timesteps()], [calc_O2sat()],
#'     [calc_water_height()], and the built-in example datasets.
#'   \item Summarize and check data with [flag_z()],
#'     [calc_exceedance_prob()], [calc_cv()], and [calc_bin_width()].
#' }
#'
#' @section Built-in datasets:
#' [french_creek] contains 5-minute dissolved oxygen and water temperature
#' observations for a complete metabolism-preparation example.
#' [kings_discharge] contains daily USGS discharge, gage height, and water
#' temperature observations for flow-duration and seasonal summaries.
#'
#' @seealso
#' [streamMetabolizer::metab()],
#' <https://connorb.github.io/preMetabolizer/>, and
#' <https://github.com/ConnorB/preMetabolizer/issues>.
#' @docType package
#' @name preMetabolizer-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib preMetabolizer, .registration = TRUE
## usethis namespace: end
NULL
