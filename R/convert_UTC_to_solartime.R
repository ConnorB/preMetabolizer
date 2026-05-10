#' Convert UTC time to solar time
#'
#' Converts a UTC datetime to mean or apparent solar time for a given longitude.
#' Uses a high-precision offset for longitude and applies the equation of time
#' for apparent solar time.
#'
#' @param date.time POSIXct vector in UTC.
#' @param longitude Numeric longitude in decimal degrees. Western longitudes
#'   are negative.
#' @param time.type One of `"mean solar"` (default) or `"apparent solar"`.
#'
#' @details
#' Apparent solar time, or true solar time, is noon when the sun is at its
#' zenith. Mean solar time approximates apparent solar time but keeps noons
#' exactly 24 hours apart. Elsewhere in this package, variables named
#' `solar.time` are mean solar time.
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
    cli::cli_abort("{.arg date.time} must be a POSIXct vector.")
  }
  if (
    !(attr(date.time, "tzone") %in% c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))
  ) {
    cli::cli_abort("{.arg date.time} must have timezone {.val UTC}.")
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
