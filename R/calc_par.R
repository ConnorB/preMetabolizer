#' Calculate modeled photosynthetically active radiation
#'
#' Calculate photosynthetically active radiation (PAR) for a series of
#' date-times and site coordinates. Input times should be mean solar time, as
#' expected by stream metabolism models. Renamed from `calc_light()` to avoid
#' shadowing `streamMetabolizer::calc_light()`.
#'
#' This function was adapted from
#' `streamMetabolizer::calc_light` and is included here under the terms of the
#' CC0 1.0 Universal public domain dedication, as described at:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' Original citation:
#' Appling, A. P., Hall, R. O., Yackulic, C. B., & Arroita, M. (2018).
#' Overcoming Equifinality: Leveraging Long Time Series for Stream Metabolism
#' Estimation. *Journal of Geophysical Research: Biogeosciences*, 123(2), 624-645.
#' https://doi.org/10.1002/2017JG004140
#'
#' The original implementation routed PAR through SW and back via the
#' `LakeMetabolizer::par.to.sw.base()` / `sw.to.par.base()` linear factors
#' (`par * 0.473` and `sw * 2.114`). Because those factors are reciprocals
#' the round-trip cancels exactly, so the conversion has been inlined and
#' the `streamMetabolizer` / `LakeMetabolizer` dependency removed from this
#' function.
#'
#' @param solar_time POSIXct vector of mean solar time values.
#' @param latitude Numeric. Site latitude in decimal degrees between -90 and
#'   90.
#' @param longitude Numeric. Site longitude in decimal degrees; western
#'   longitudes are negative.
#' @param max_par Numeric. Peak daily PAR in umol m^-2 s^-1. Defaults to
#'   `2326`.
#'
#' @return Numeric vector of modeled PAR in umol m^-2 s^-1.
#'
#' @details
#' The solar zenith angle at each timestamp is computed via
#' [SunCalcMeeus::sun_zenith_angle()] (Meeus's algorithms), and PAR is
#' modeled as `max_par * cos(zenith)` clipped at zero. The mean solar input
#' is converted back to UTC for the zenith calculation via the standard
#' 15 deg / hour longitude offset.
#'
#' @references
#' Britton, C. M. and Dodd, J. D. (1976). Relationships of photosynthetically
#' active radiation and shortwave irradiance. *Agricultural Meteorology*,
#' 17(1), 1-7. (Source of the 0.473 W m^-2 per umol m^-2 s^-1 PAR-SW factor.)
#'
#' @examples
#' utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
#' solar_time <- convert_to_solar_time(utc, longitude = -96.6)
#'
#' calc_par(solar_time, latitude = 39.1, longitude = -96.6)
#'
#' @export
calc_par <- function(solar_time, latitude, longitude, max_par = 2326) {
  utc <- convert_from_solar_time(
    solar_time,
    longitude = longitude,
    type = "mean"
  )

  geocode <- tibble::tibble(
    lon = longitude,
    lat = latitude,
    address = "calc_par_site"
  )
  zenith_rad <- SunCalcMeeus::sun_zenith_angle(
    time = utc,
    geocode = geocode
  ) *
    pi /
    180

  pmax(max_par * cos(zenith_rad), 0)
}
