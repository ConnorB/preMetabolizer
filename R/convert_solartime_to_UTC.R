#' Convert solar time to UTC
#'
#' Converts a datetime from local solar time back to UTC. Input time may be
#' either apparent solar time or mean solar time.
#'
#' @param any.solar.time POSIXct vector in solar time. The timezone attribute
#'   must be UTC, even though the clock time represents solar time.
#' @param longitude Numeric longitude in decimal degrees. Western longitudes
#'   are negative.
#' @param time.type Character string: either `"mean solar"` or
#'   `"apparent solar"`.
#'
#' @return A POSIXct datetime in UTC.
#'
#' @examples
#' utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
#' solar <- convert_UTC_to_solartime(
#'   utc,
#'   longitude = -96.6,
#'   time.type = "mean solar"
#' )
#'
#' convert_solartime_to_UTC(
#'   solar,
#'   longitude = -96.6,
#'   time.type = "mean solar"
#' )
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
    cli::cli_abort("{.arg any.solar.time} must be a POSIXct vector.")
  }
  if (
    !(attr(any.solar.time, "tzone") %in%
      c("UTC", "GMT", "Etc/GMT-0", "Etc/GMT+0"))
  ) {
    cli::cli_abort("{.arg any.solar.time} must have timezone {.val UTC}.")
  }

  # Get solar-to-UTC offset (the inverse of the UTC-to-solar transformation)
  conversion <- convert_UTC_to_solartime(any.solar.time, longitude, time.type) -
    any.solar.time
  utc_time <- any.solar.time - conversion

  return(utc_time)
}
