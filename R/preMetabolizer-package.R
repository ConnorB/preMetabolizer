#' preMetabolizer: Prepare data for stream metabolism modeling
#'
#' preMetabolizer provides helpers for building the environmental time series
#' commonly needed before fitting stream metabolism models. It includes tools
#' to download external meteorological and elevation data, align irregular time
#' series, convert units, calculate water and gas chemistry quantities, and
#' assemble light and pressure inputs for streamMetabolizer.
#'
#' @section Coordinates:
#' Functions that require locations use `latitude` and `longitude` in decimal
#' degrees. Latitude ranges from -90 to 90, longitude ranges from -180 to 180,
#' and western longitudes are negative.
#'
#' @section Main workflows:
#' \itemize{
#'   \item Download site-level environmental data with [get_nasa_data()],
#'     [get_usgs_elev()], [get_noaa_stations()], and [get_ks_meso()].
#'   \item Prepare regularly spaced time series with [even_timesteps()] and
#'     [get_season()].
#'   \item Calculate light, barometric pressure, dissolved oxygen saturation,
#'     water density, stream depth, and dissolved carbon dioxide inputs.
#'   \item Convert common hydrology and atmospheric units with
#'     [convert_flow()] and [convert_pressure()].
#' }
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
