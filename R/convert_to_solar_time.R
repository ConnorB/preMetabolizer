#' Convert a datetime to local solar time
#'
#' Converts a datetime to mean or apparent (true) solar time at a given
#' longitude. Mean solar time uses a constant 15 deg/hour longitude offset.
#' Apparent solar time additionally applies the equation of time and is
#' computed via [SunCalcMeeus::solar_time()].
#'
#' These helpers are named `convert_to_solar_time()` /
#' `convert_from_solar_time()` to avoid shadowing the original
#' `streamMetabolizer::convert_UTC_to_solartime()` and
#' `streamMetabolizer::convert_solartime_to_UTC()`, which can still be called
#' directly from `streamMetabolizer` if you need their behaviour.
#'
#' @param dateTime A datetime vector. POSIXct in any time zone is accepted;
#'   the function operates on the underlying instant, so a CDT timestamp
#'   and the matching UTC timestamp produce identical results. Character or
#'   `Date` input is coerced with `as.POSIXct(., tz = "UTC")`.
#' @param solar_datetime A POSIXct vector of solar-time values. The clock
#'   reading is interpreted as solar time even though the tzone attribute is
#'   UTC; this matches the convention used by `streamMetabolizer` and
#'   `SunCalcMeeus`.
#' @param longitude Numeric. Site longitude in decimal degrees; western
#'   longitudes are negative.
#' @param type One of `"mean"` (default) or `"apparent"`.
#'
#' @return A POSIXct vector with class `c("solar_date", "POSIXct", "POSIXt")`
#'   (`convert_to_solar_time()`) or `c("POSIXct", "POSIXt")`
#'   (`convert_from_solar_time()`). The tzone attribute is `"UTC"` in both
#'   cases; the clock reading carries the meaning.
#'
#' @details
#' The mean offset is `longitude / 15 * 3600` seconds. The apparent path
#' delegates to [SunCalcMeeus::solar_time()], which applies the equation of
#' time using Meeus's algorithms.
#'
#' `convert_from_solar_time()` inverts the forward conversion by computing
#' the forward offset *at the input solar instant* and subtracting it. This
#' is exact for `type = "mean"`. For `type = "apparent"` it is accurate to
#' within roughly the daily change in the equation of time (~30 s/day), which
#' is well below typical sensor sampling intervals.
#'
#' @examples
#' utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
#'
#' convert_to_solar_time(utc, longitude = -96.6)
#' convert_to_solar_time(utc, longitude = -96.6, type = "apparent")
#'
#' # A non-UTC POSIXct gives the same result, since the underlying instant
#' # is what matters.
#' local <- as.POSIXct("2024-06-21 13:00:00", tz = "America/Chicago")
#' convert_to_solar_time(local, longitude = -96.6)
#'
#' solar <- convert_to_solar_time(utc, longitude = -96.6)
#' convert_from_solar_time(solar, longitude = -96.6)
#'
#' @seealso [SunCalcMeeus::solar_time()],
#'   `streamMetabolizer::convert_UTC_to_solartime()`,
#'   `streamMetabolizer::convert_solartime_to_UTC()`.
#'
#' @export
convert_to_solar_time <- function(
  dateTime,
  longitude,
  type = c("mean", "apparent")
) {
  type <- match.arg(type)

  if (!inherits(dateTime, "POSIXct")) {
    dateTime <- as.POSIXct(dateTime, tz = "UTC")
  } else {
    dateTime <- lubridate::with_tz(dateTime, "UTC")
  }

  if (type == "mean") {
    offset_seconds <- longitude / 15 * 3600
    solar <- dateTime + offset_seconds
    attr(solar, "tzone") <- "UTC"
    class(solar) <- c("solar_date", "POSIXct", "POSIXt")
    return(solar)
  }

  geocode <- tibble::tibble(
    lon = longitude,
    lat = 0,
    address = "solar_time_reference"
  )

  raw_solar <- SunCalcMeeus::solar_time(
    time = dateTime,
    geocode = geocode,
    unit.out = "datetime"
  )

  # `SunCalcMeeus::solar_time()` returns the solar time wrapped onto the
  # same calendar day as the UTC input (i.e., it adds or removes 24 h to
  # keep the clock reading within [00:00, 24:00) on the input date). Undo
  # that wrap so the returned timestamp lies on the correct solar
  # calendar day. The natural offset is bounded by abs(longitude)/15
  # hours, so normalising to [-12 h, +12 h] is sufficient.
  offset <- as.numeric(raw_solar) - as.numeric(dateTime)
  offset <- ((offset + 43200) %% 86400) - 43200
  solar <- dateTime + offset
  attr(solar, "tzone") <- "UTC"
  class(solar) <- c("solar_date", "POSIXct", "POSIXt")
  solar
}

#' @rdname convert_to_solar_time
#' @export
convert_from_solar_time <- function(
  solar_datetime,
  longitude,
  type = c("mean", "apparent")
) {
  type <- match.arg(type)

  if (!inherits(solar_datetime, "POSIXct")) {
    solar_datetime <- as.POSIXct(solar_datetime, tz = "UTC")
  } else {
    solar_datetime <- lubridate::with_tz(solar_datetime, "UTC")
  }

  if (type == "mean") {
    offset_seconds <- longitude / 15 * 3600
    utc <- solar_datetime - offset_seconds
  } else {
    # Approximate inverse: compute the forward offset at the input solar
    # instant and subtract. Accuracy is bounded by the daily change in the
    # equation of time (~30 s/day).
    forward <- convert_to_solar_time(
      solar_datetime,
      longitude = longitude,
      type = "apparent"
    )
    offset <- as.numeric(forward) - as.numeric(solar_datetime)
    utc <- solar_datetime - offset
  }

  attr(utc, "tzone") <- "UTC"
  class(utc) <- c("POSIXct", "POSIXt")
  utc
}
