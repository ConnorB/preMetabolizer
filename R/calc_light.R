#' Calculate modeled photosynthetically active radiation
#'
#' Calculate photosynthetically active radiation (PAR) for a series of
#' date-times and site coordinates. Input times should be mean solar time, as
#' expected by stream metabolism models.
#'
#' This function was adapted from
#' `streamMetabolizer::calc_light` and is included here under the terms of the
#' CC0 1.0 Universal public domain dedication, as described at:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' Original citation:
#' Appling, A. P., Hall, R. O., Yackulic, C. B., & Arroita, M. (2018).
#' Overcoming Equifinality: Leveraging Long Time Series for Stream Metabolism
#' Estimation. *Journal of Geophysical Research: Biogeosciences*, 123(2), 624–645.
#' https://doi.org/10.1002/2017JG004140
#'
#' @param solar.time POSIXct vector of mean solar time values.
#' @param latitude Numeric. Site latitude in decimal degrees between -90 and
#'   90.
#' @inheritParams convert_solartime_to_UTC
#' @param max.PAR Numeric. Peak daily PAR in umol m^-2 s^-1. Defaults to
#'   `2326`.
#' @inheritParams calc_solar_insolation
#'
#' @return Numeric vector of modeled PAR in umol m^-2 s^-1.
#'
#' @examples
#' utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
#' solar.time <- convert_UTC_to_solartime(utc, longitude = -96.6)
#'
#' calc_light(solar.time, latitude = 39.1, longitude = -96.6)
#'
#' @export
calc_light <- function(
  solar.time,
  latitude,
  longitude,
  max.PAR = 2326
) {
  coef.SW.to.PAR <- formals(streamMetabolizer::convert_SW_to_PAR)$coef
  app.solar.time <- solar.time |>
    convert_solartime_to_UTC(longitude = longitude, time.type = 'mean solar') |>
    convert_UTC_to_solartime(
      longitude = longitude,
      time.type = 'apparent solar'
    )
  sw <- calc_solar_insolation(
    app.solar.time,
    latitude = latitude,
    max.insolation = streamMetabolizer::convert_PAR_to_SW(
      max.PAR,
      coef = 1 / coef.SW.to.PAR
    ),
    format = c("degrees", "radians")
  )
  par <- streamMetabolizer::convert_SW_to_PAR(sw, coef = coef.SW.to.PAR)

  pmax(par, 0)
}
