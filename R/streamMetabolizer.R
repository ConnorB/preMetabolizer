#' Calculate modeled light from solar.time
#'
#' Calculate photosynthetically active radiation (PAR) for a series of
#' date-times and site coordinates. This function was adapted from
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
#' @param solar.time mean solar time, as required for input to metabolism
#'   models.
#' @param latitude numeric. Site latitude in decimal degrees between -90 and 90.
#' @inheritParams convert_solartime_to_UTC
#' @param max.PAR numeric: the PAR (umol m^-2 s^-1) that each
#'   day should reach at peak light
#' @inheritParams calc_solar_insolation
#' @examples
#' solar.time <- lubridate::force_tz(as.POSIXct('2016-09-27 12:00'), 'UTC')
#' calc_light(solar.time, 40, -120)
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

#' Convert UTC Time to Mean or Apparent Solar Time
#'
#' Converts a UTC datetime to mean or apparent solar time for a given longitude.
#' Uses a high-precision offset for longitude and applies the equation of time
#' for apparent solar time.
#'
#' @param date.time A POSIXct datetime object in UTC timezone.
#' @param longitude Longitude in decimal degrees (east positive, west negative).
#' @param time.type One of `"mean solar"` (default) or `"apparent solar"`.
#'
#' @details
#' "apparent solar", i.e. true solar time, is noon when the sun is at its zenith.
#' "mean solar" approximates apparent solar time but with noons exactly 24 hours apart.
#' Elsewhere in this package, variables named `solar.time` are mean solar time.
#'
#' @return A POSIXct datetime in mean or apparent solar time (still tz = "UTC").
#'
#' @examples
#' utc <- as.POSIXct("2025-04-30 12:00:00", tz = "UTC")
#' convert_UTC_to_solartime(utc, -90, time.type = "mean solar")
#' convert_UTC_to_solartime(utc, -90, time.type = "apparent solar")
#'
#' @export
convert_UTC_to_solartime <- function(
  date.time,
  longitude,
  time.type = c("mean solar", "apparent solar")
) {
  time.type <- match.arg(time.type)

  if (!inherits(date.time, "POSIXct")) {
    stop("date.time must be a POSIXct object")
  }
  if (
    !(attr(date.time, "tzone") %in% c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))
  ) {
    stop("date.time must have tz = 'UTC'")
  }

  # Mean solar adjustment: 3.989 minutes per degree longitude
  mean_offset_sec <- longitude * 3.989 * 60
  mean_solar_time <- date.time + mean_offset_sec

  if (time.type == "mean solar") {
    return(mean_solar_time)
  }

  # Apparent solar time adjustment using the Equation of Time (in minutes)
  doy <- as.numeric(strftime(mean_solar_time, "%j")) - 1
  B <- 2 * pi * (doy - 81) / 365
  eq_time_min <- 9.87 * sin(2 * B) - 7.53 * cos(B) - 1.5 * sin(B)
  apparent_solar_time <- mean_solar_time + eq_time_min * 60

  return(apparent_solar_time)
}

#' Convert DateTime from Local Solar Time to UTC
#'
#' Converts a datetime from local solar time back to UTC. Input time may be either
#' apparent solar time (sun at zenith at noon) or mean solar time (noons exactly 24 hours apart).
#'
#' @param any.solar.time A POSIXct datetime object in solar time (mean or apparent). Must have tz = "UTC".
#' @param longitude Numeric longitude in decimal degrees (east positive, west negative).
#' @param time.type Character string: either `"mean solar"` or `"apparent solar"`.
#'   "apparent solar", i.e. true solar time, is noon when the sun is at its zenith.
#'   "mean solar" approximates apparent solar time but with noons exactly 24 hours apart.
#'
#' @return A POSIXct datetime in UTC.
#'
#' @export
#' @references Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
#'   Influence of topographic complexity on solar insolation estimates for the
#'   Colorado River, Grand Canyon, AZ. Ecological Modelling.
convert_solartime_to_UTC <- function(
  any.solar.time,
  longitude,
  time.type = c("apparent solar", "mean solar")
) {
  time.type <- match.arg(time.type)

  if (!inherits(any.solar.time, "POSIXct")) {
    stop("any.solar.time must be a POSIXct object")
  }
  if (
    !(attr(any.solar.time, "tzone") %in%
      c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))
  ) {
    stop("expecting nominal tz='UTC' for solar.time")
  }

  # Get solar-to-UTC offset (the inverse of the UTC-to-solar transformation)
  conversion <- convert_UTC_to_solartime(any.solar.time, longitude, time.type) -
    any.solar.time
  utc_time <- any.solar.time - conversion

  return(utc_time)
}

#' Model solar insolation on a horizontal surface (W/m2 == J/s/m2) as in
#' http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html
#'
#' Adapted from `streamMetabolizer`, included under the CC0 1.0 Universal public domain dedication:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' @param app.solar.time POSIXct vector of date-time values in apparent solar
#'   time, e.g., as returned by `convert_UTC_to_solartime(...,
#'   time.type="apparent solar")`
#' @param latitude numeric. Site latitude in decimal degrees between -90 and 90.
#' @param format character. `"degrees"` or `"radians"`.
#' @param max.insolation insolation rate at solar noon, W/m2 == J/s/m2. varies
#'   greatly with atmospheric conditions
calc_solar_insolation <- function(
  app.solar.time,
  latitude,
  max.insolation = streamMetabolizer::convert_PAR_to_SW(2326),
  format = c("degrees", "radians")
) {
  format <- match.arg(format)
  time.posix <- as.POSIXlt(app.solar.time, tz = "UTC")

  jday <- time.posix$yday
  hour <- time.posix$hour + time.posix$min / 60 + time.posix$sec / 3600

  declination.angle <- streamMetabolizer:::calc_declination_angle(
    jday,
    format = format
  )
  hour.angle <- streamMetabolizer:::calc_hour_angle(hour, format = format)
  zenith.angle <- streamMetabolizer:::calc_zenith_angle(
    latitude,
    declination.angle,
    hour.angle,
    format = format
  )

  if (format == "degrees") {
    zenith.angle <- streamMetabolizer:::to_radians(zenith.angle)
  }
  insolation <- max.insolation * cos(zenith.angle)

  return(insolation)
}
