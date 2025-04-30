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
#' @inheritParams calc_zenith_angle
#' @inheritParams convert_solartime_to_UTC
#' @param max.PAR numeric: the PAR (umol m^-2 s^-1) that each
#'   day should reach at peak light
#' @inheritParams calc_solar_insolation
#' @import dplyr
#' @examples
#' solar.time <- lubridate::force_tz(as.POSIXct('2016-09-27 12:00'), 'UTC')
#' calc_light(solar.time, 40, -120)
#' @export
calc_light <- function(
    solar.time, latitude, longitude, max.PAR = 2326) {


  coef.SW.to.PAR <- formals(convert_SW_to_PAR)$coef
  app.solar.time <- solar.time |>
    convert_solartime_to_UTC(longitude=longitude, time.type='mean solar') |>
    convert_UTC_to_solartime(longitude=longitude, time.type='apparent solar')
  sw <- calc_solar_insolation(
    app.solar.time, latitude=latitude,
    max.insolation=convert_PAR_to_SW(max.PAR, coef=1/coef.SW.to.PAR),
    format=c("degrees", "radians"))
  par <- convert_SW_to_PAR(sw, coef=coef.SW.to.PAR)

  par
}

#' Convert from photosynthetically active to shortwave radiation
#'
#' Convert photosynthetically active radiation (PAR) to shortwave radiation
#' (SW). Uses a fixed ratio between PAR and SW, ignoring the minor seasonal
#' changes in this ratio (see Britton and Dodd (1976)).
#'
#' This function was adapted from `streamMetabolizer::convert_PAR_to_SW`,
#' which wraps a base conversion from `LakeMetabolizer::par.to.sw.base`. It is
#' included under the terms of the CC0 1.0 Universal public domain dedication:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' Original citation:
#' Appling, A. P., Hall, R. O., Yackulic, C. B., & Arroita, M. (2018).
#' Overcoming Equifinality: Leveraging Long Time Series for Stream Metabolism
#' Estimation. *Journal of Geophysical Research: Biogeosciences*, 123(2), 624–645.
#' https://doi.org/10.1002/2017JG004140
#'
#' @param par Vector of photosynthetically active radiation (400-700 nm;
#'   umol/m^2/sec)
#' @param coef Numerical coefficient to convert PAR (umol/m^2/sec) to SW
#'   (W/m^2). Defaults to value from Britton and Dodd (1976).
#' @return Numeric vector of shortwave values with units W/m^2
#' @examples
#' convert_PAR_to_SW(par=400, coef=0.47)
#' @importFrom LakeMetabolizer par.to.sw.base
#' @export
convert_PAR_to_SW <- function(par, coef=0.473) {
  LakeMetabolizer::par.to.sw.base(par, coef)
}

#' Convert from shortwave to photosynthetically active radiation
#'
#' Convert shortwave radiation (SW) to photosynthetically active radiation
#' (PAR). Uses a fixed ratio between SW and PAR, ignoring the minor seasonal
#' changes in this ratio (see Britton and Dodd (1976)).
#'
#' This function was adapted from `streamMetabolizer::convert_SW_to_PAR`,
#' which wraps `LakeMetabolizer::sw.to.par.base`. It is included under the terms
#' of the CC0 1.0 Universal public domain dedication:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' Original citation:
#' Appling, A. P., Hall, R. O., Yackulic, C. B., & Arroita, M. (2018).
#' Overcoming Equifinality: Leveraging Long Time Series for Stream Metabolism
#' Estimation. *Journal of Geophysical Research: Biogeosciences*, 123(2), 624–645.
#' https://doi.org/10.1002/2017JG004140
#'
#' @param sw Vector of shortwave radiation (W/m^2)
#' @param coef Numerical coefficient to convert SW (W/m^2) to PAR
#'   (umol/m^2/sec). Defaults to value from Britton and Dodd (1976).
#' @return Numeric vector of PAR values in units umol/m^2/sec
#' @examples
#' convert_SW_to_PAR(sw=800)
#' convert_SW_to_PAR(sw=800, coef=2.1)
#' @importFrom LakeMetabolizer sw.to.par.base
#' @export
convert_SW_to_PAR <- function(sw, coef=2.114) {
  LakeMetabolizer::sw.to.par.base(sw, coef)
}


#' Convert degrees to radians
#'
#' Converts degrees to radians.
#'
#' Adapted from `streamMetabolizer`, included under the CC0 1.0 Universal public domain dedication:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' @param degrees angle in degrees
#' @return angle in radians
to_radians <- function(degrees) {
  degrees * pi / 180
}

#' Convert radians to degrees
#'
#' Converts radians to degrees.
#'
#' Adapted from `streamMetabolizer`, included under the CC0 1.0 Universal public domain dedication.
#'
#' @param radians angle in radians
#' @return angle in degrees
to_degrees <- function(radians) {
  radians * 180 / pi
}

#' Calculate hour angle as in
#' http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html.
#'
#' Adapted from `streamMetabolizer::calc_hour_angle` under CC0 license.
#'
#' This is an approximation when hour is in clock time; should actually be given
#' in solar time
#'
#' @param hour numeric value or vector. hour since (solar) midnight as number
#'   between 0 and 23.999
#' @param format The format of both the input and the output. May be "degrees"
#'   or "radians".
#' @return numeric value or vector, in the units specified by \code{format},
#'   indicating the angle corresponding to each value supplied in \code{hour}.
calc_hour_angle <- function(hour, format=c("degrees", "radians")) {
  format <- match.arg(format)
  hour.angle <- (360/24)*(hour-12)
  if(format=="radians") hour.angle <- to_radians(hour.angle)
  hour.angle
}

#' Calculate zenith angle as in
#' http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html
#'
#' Adapted from `streamMetabolizer::calc_hour_angle` under CC0 license.
#'
#' @param latitude numeric value or vector indicating the site latitude in
#'   decimal degrees (never radians or deg-min-sec, no matter what \code{format}
#'   is) between -90 (South Pole) and 90 (North Pole).
#' @param declination.angle numeric value or vector, in the units specified by
#'   \code{format}, indicating the declination angle.
#' @param hour.angle numeric value or vector, in the units specified by
#'   \code{format}, indicating the angle.
#' @param format The format of both the output. May be "degrees" or "radians".
calc_zenith_angle <- function(latitude, declination.angle, hour.angle, format=c("degrees", "radians")) {
  format <- match.arg(format)

  latitude <- to_radians(latitude)
  if(format == "degrees") {
    declination.angle <- to_radians(declination.angle)
    hour.angle <- to_radians(hour.angle)
  }
  zenith.angle <-
    acos(sin(latitude) * sin(declination.angle) +
           cos(latitude) * cos(declination.angle) * cos(hour.angle))
  if(format == "degrees") {
    zenith.angle <- to_degrees(zenith.angle)
  }
  zenith.angle
}

#' Convert UTC Time to Mean or Apparent Solar Time
#'
#' Converts a UTC datetime to mean or apparent solar time for a given longitude.
#' Uses a high-precision offset for longitude and applies the equation of time
#' for apparent solar time.
#'
#' @param date.time A POSIXct datetime object in UTC timezone.
#' @param longitude Longitude in decimal degrees (east positive, west negative).
#' @param time.type One of \code{"mean solar"} (default) or \code{"apparent solar"}.
#'
#' @details
#' "apparent solar", i.e. true solar time, is noon when the sun is at its zenith.
#' "mean solar" approximates apparent solar time but with noons exactly 24 hours apart.
#' Elsewhere in this package, variables named \code{solar.time} are mean solar time.
#'
#' @return A POSIXct datetime in mean or apparent solar time (still tz = "UTC").
#'
#' @examples
#' utc <- as.POSIXct("2025-04-30 12:00:00", tz = "UTC")
#' convert_UTC_to_solartime(utc, -90, time.type = "mean solar")
#' convert_UTC_to_solartime(utc, -90, time.type = "apparent solar")
#'
#' @export
convert_UTC_to_solartime <- function(date.time, longitude, time.type = c("mean solar", "apparent solar")) {
  time.type <- match.arg(time.type)

  if (!inherits(date.time, "POSIXct")) stop("date.time must be a POSIXct object")
  if (!(attr(date.time, "tzone") %in% c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))) {
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
#' @param time.type Character string: either \code{"mean solar"} or \code{"apparent solar"}.
#'   "apparent solar", i.e. true solar time, is noon when the sun is at its zenith.
#'   "mean solar" approximates apparent solar time but with noons exactly 24 hours apart.
#'
#' @return A POSIXct datetime in UTC.
#'
#' @export
#' @references Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
#'   Influence of topographic complexity on solar insolation estimates for the
#'   Colorado River, Grand Canyon, AZ. Ecological Modelling.
convert_solartime_to_UTC <- function(any.solar.time, longitude, time.type = c("apparent solar", "mean solar")) {
  time.type <- match.arg(time.type)

  if (!inherits(any.solar.time, "POSIXct")) stop("any.solar.time must be a POSIXct object")
  if (!(attr(any.solar.time, "tzone") %in% c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))) {
    stop("expecting nominal tz='UTC' for solar.time")
  }

  # Get solar-to-UTC offset (the inverse of the UTC-to-solar transformation)
  conversion <- convert_UTC_to_solartime(any.solar.time, longitude, time.type) - any.solar.time
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
#'   time, e.g., as returned by \code{convert_UTC_to_solartime(...,
#'   time.type="apparent solar")}
#' @inheritParams calc_declination_angle
#' @inheritParams calc_hour_angle
#' @inheritParams calc_zenith_angle
#' @param max.insolation insolation rate at solar noon, W/m2 == J/s/m2. varies
#'   greatly with atmospheric conditions
calc_solar_insolation <- function(
    app.solar.time, latitude, max.insolation = convert_PAR_to_SW(2326),
    format = c("degrees", "radians")) {

  format <- match.arg(format)
  time.posix <- as.POSIXlt(app.solar.time, tz = "UTC")

  # Day of year: 0-based (0 = Jan 1)
  jday <- time.posix$yday

  # Decimal hour
  hour <- time.posix$hour + time.posix$min / 60 + time.posix$sec / 3600

  # Compute angles
  declination.angle <- calc_declination_angle(jday, format = format)
  hour.angle <- calc_hour_angle(hour, format = format)
  zenith.angle <- calc_zenith_angle(latitude, declination.angle, hour.angle, format = format)

  if (format == "degrees") zenith.angle <- to_radians(zenith.angle)
  insolation <- max.insolation * cos(zenith.angle)

  return(insolation)
}


#' Calculate declination angle as in Yard et al. (2005)
#'
#'#' Adapted from `streamMetabolizer`, included under the CC0 1.0 Universal public domain dedication:
#' https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits
#'
#' @param jday The day of year as a number between 0 (Jan 1) and 364 (365 also
#'   OK for leap year)
#' @param format The format of both the input and the output. May be "degrees"
#'   or "radians".
#' @return numeric value or vector, in the units specified by \code{format},
#'   indicating the declination angle corresponding to each value supplied in
#'   \code{jday}.
#' @examples
#' decdf <- data.frame(jday=1:366,
#'   dec=preMetabolizer:::calc_declination_angle(1:366))
#' @references Yard, Michael D., Glenn E. Bennett, Steve N. Mietz, Lewis G.
#'   Coggins Jr., Lawrence E. Stevens, Susan Hueftle, and Dean W. Blinn.
#'   \emph{Influence of Topographic Complexity on Solar Insolation Estimates for
#'   the Colorado River, Grand Canyon, AZ.} Ecological Modelling 183, no. 2-3
#'   (April 25, 2005): 157-72. doi:10.1016/j.ecolmodel.2004.07.027.
calc_declination_angle <- function(jday, format=c("degrees", "radians")) {
  format <- match.arg(format)
  declination.angle <- 23.439*sin(to_radians((360/365)*(283+jday)))
  if(format == "radians") {
    declination.angle <- to_radians(declination.angle)
  }
  declination.angle
}

