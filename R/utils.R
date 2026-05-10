#' Get the last modified time of a remote file
#'
#' This internal function retrieves the "Last-Modified" timestamp of a remote
#' file by sending an HTTP HEAD request to the given URL. The result is
#' memoised to cache repeated calls with the same URL.
#'
#' @param url A character string specifying the URL of the remote file.
#'
#' @return A POSIXlt object representing the last modified timestamp of the
#'   remote file, or `NULL` if the information is unavailable or an error
#'   occurs.
#'
#' @details The function sends an HTTP HEAD request to the specified URL using
#'   the \pkg{httr2} package. If the server responds with a 200 status code and
#'   includes a "Last-Modified" header, the timestamp is parsed and returned. If
#'   the request fails or the "Last-Modified" header is missing, `NULL` is
#'   returned.
#'
#' @note The function uses \pkg{memoise} to cache results, so repeated calls
#'   with the same URL will not trigger additional HTTP requests.
#'
#' @noRd
get_remote_mtime <- memoise::memoise(function(url) {
  tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        http_req_perform()
      if (httr2::resp_status(resp) == 200) {
        file_date <- httr2::resp_header(resp, "last-modified")
        file_date <- strptime(file_date, "%a, %d %b %Y %H:%M:%S", tz = "GMT")
        return(file_date)
      }
      NULL
    },
    error = function(e) NULL
  )
})

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
#' @noRd
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

  declination.angle <- calc_declination_angle(
    jday,
    format = format
  )
  hour.angle <- calc_hour_angle(hour, format = format)
  zenith.angle <- calc_zenith_angle(
    latitude,
    declination.angle,
    hour.angle,
    format = format
  )

  if (format == "degrees") {
    zenith.angle <- to_radians(zenith.angle)
  }
  insolation <- max.insolation * cos(zenith.angle)

  return(insolation)
}


#' Calculate the solar declination angle
#'
#' Computes the solar declination angle for a given Julian day using an
#' approximation based on the Earth's axial tilt and orbital position.
#' The declination angle describes the angular position of the sun relative
#' to the Earth's equatorial plane.
#'
#' @param jday Numeric vector of Julian day values (1--365/366).
#' @param format Character string indicating the desired output units.
#'   Either `"degrees"` (default) or `"radians"`.
#'
#' @return A numeric vector containing solar declination angles in the
#'   requested units.
#'
#' @details
#' The declination angle is approximated as:
#'
#' \deqn{
#' \delta = 23.439 \sin\left(\frac{360}{365}(283 + jday)\right)
#' }
#'
#' where \eqn{\delta} is initially computed in degrees before optional
#' conversion to radians.
#'
#' @seealso [calc_hour_angle()], [calc_zenith_angle()]
#'
#' @noRd
calc_declination_angle <- function(jday, format = c("degrees", "radians")) {
  format <- match.arg(format)
  declination.angle <- 23.439 * sin(to_radians((360 / 365) * (283 + jday)))

  if (format == "radians") {
    declination.angle <- to_radians(declination.angle)
  }

  declination.angle
}

#' Calculate the solar hour angle
#'
#' Computes the solar hour angle from local solar time. The hour angle
#' represents the angular displacement of the sun east or west of the
#' local solar meridian.
#'
#' @param hour Numeric vector representing hour values in local solar time.
#' @param format Character string indicating the desired output units.
#'   Either `"degrees"` (default) or `"radians"`.
#'
#' @return A numeric vector containing solar hour angles in the requested
#'   units.
#'
#' @details
#' The hour angle is calculated as:
#'
#' \deqn{
#' h = \frac{360}{24}(hour - 12)
#' }
#'
#' where solar noon corresponds to an hour angle of 0 degrees.
#'
#' @seealso [calc_declination_angle()], [calc_zenith_angle()]
#'
#' @noRd
calc_hour_angle <- function(hour, format = c("degrees", "radians")) {
  format <- match.arg(format)
  hour.angle <- (360 / 24) * (hour - 12)

  if (format == "radians") {
    hour.angle <- to_radians(hour.angle)
  }

  hour.angle
}

#' Calculate the solar zenith angle
#'
#' Computes the solar zenith angle from latitude, solar declination angle,
#' and solar hour angle using spherical trigonometry.
#'
#' @param latitude Numeric vector of geographic latitude values in degrees.
#' @param declination.angle Numeric vector of solar declination angles.
#' @param hour.angle Numeric vector of solar hour angles.
#' @param format Character string indicating the units of
#'   `declination.angle` and `hour.angle`, as well as the desired output
#'   units. Either `"degrees"` (default) or `"radians"`.
#'
#' @return A numeric vector containing solar zenith angles in the requested
#'   units.
#'
#' @details
#' The solar zenith angle is calculated as:
#'
#' \deqn{
#' \theta_z =
#' \arccos(
#' \sin(\phi)\sin(\delta) +
#' \cos(\phi)\cos(\delta)\cos(h)
#' )
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{\theta_z} is the solar zenith angle,
#'   \item \eqn{\phi} is latitude,
#'   \item \eqn{\delta} is solar declination angle,
#'   \item \eqn{h} is solar hour angle.
#' }
#'
#' Latitude values are always assumed to be provided in degrees and are
#' internally converted to radians.
#'
#' @seealso [calc_declination_angle()], [calc_hour_angle()]
#'
#' @noRd
calc_zenith_angle <- function(
  latitude,
  declination.angle,
  hour.angle,
  format = c("degrees", "radians")
) {
  format <- match.arg(format)
  latitude <- to_radians(latitude)

  if (format == "degrees") {
    declination.angle <- to_radians(declination.angle)
    hour.angle <- to_radians(hour.angle)
  }

  zenith.angle <- acos(
    sin(latitude) *
      sin(declination.angle) +
      cos(latitude) * cos(declination.angle) * cos(hour.angle)
  )

  if (format == "degrees") {
    zenith.angle <- to_degrees(zenith.angle)
  }

  zenith.angle
}

#' Convert degrees to radians
#'
#' Converts angular measurements from degrees to radians.
#'
#' @param degrees Numeric vector of angles in degrees.
#'
#' @return A numeric vector of angles in radians.
#'
#' @details
#' Conversion is performed as:
#'
#' \deqn{
#' radians = degrees \times \frac{\pi}{180}
#' }
#'
#' @seealso [to_degrees()]
#'
#' @noRd
to_radians <- function(degrees) {
  degrees * pi / 180
}

#' Convert radians to degrees
#'
#' Converts angular measurements from radians to degrees.
#'
#' @param radians Numeric vector of angles in radians.
#'
#' @return A numeric vector of angles in degrees.
#'
#' @details
#' Conversion is performed as:
#'
#' \deqn{
#' degrees = radians \times \frac{180}{\pi}
#' }
#'
#' @seealso [to_radians()]
#'
#' @noRd
to_degrees <- function(radians) {
  radians * 180 / pi
}
