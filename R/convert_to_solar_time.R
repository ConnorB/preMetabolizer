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
#' @param date_time A datetime vector. POSIXct in any time zone is accepted;
#'   the function operates on the underlying instant, so a CDT timestamp
#'   and the matching UTC timestamp produce identical results. Character or
#'   `Date` input is coerced with `as.POSIXct(., tz = "UTC")`.
#' @param dateTime `r lifecycle::badge("deprecated")` Use `date_time` instead.
#' @param solar_datetime A POSIXct vector of solar-time values. The clock
#'   reading is interpreted as solar time even though the tzone attribute is
#'   UTC; this matches the convention used by `streamMetabolizer` and
#'   `SunCalcMeeus`.
#' @param longitude Numeric site longitude in decimal degrees; western
#'   longitudes are negative. Length 1 for a single site, or the same length as
#'   the datetime input to convert several sites at once (one site per
#'   timestamp).
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
  date_time,
  longitude,
  type = c("mean", "apparent"),
  dateTime = lifecycle::deprecated()
) {
  type <- match.arg(type)

  if (lifecycle::is_present(dateTime)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "convert_to_solar_time(dateTime)",
      "convert_to_solar_time(date_time)"
    )
    date_time <- dateTime
  }

  if (!inherits(date_time, "POSIXct")) {
    date_time <- as.POSIXct(date_time, tz = "UTC")
  } else {
    date_time <- lubridate::with_tz(date_time, "UTC")
  }

  check_site_coord(longitude, length(date_time))

  if (type == "mean") {
    offset_seconds <- longitude / 15 * 3600
    solar <- date_time + offset_seconds
    attr(solar, "tzone") <- "UTC"
    class(solar) <- c("solar_date", "POSIXct", "POSIXt")
    return(solar)
  }

  solar <- solar_time_apparent(date_time, longitude)
  attr(solar, "tzone") <- "UTC"
  class(solar) <- c("solar_date", "POSIXct", "POSIXt")
  solar
}

# Apparent solar time for one or more sites. `SunCalcMeeus::solar_time()`
# vectorizes over time for a single geocode but cannot pair n timestamps with n
# geocode rows, so group by unique longitude and convert each site's timestamps
# together. `longitude` is length 1 (one site) or length(date_time) (one site
# per timestamp); the result preserves the input order.
solar_time_apparent <- function(date_time, longitude) {
  lon <- rep_len(longitude, length(date_time))
  out <- date_time

  for (site_lon in unique(lon)) {
    idx <- which(lon == site_lon)
    geocode <- data.frame(
      lon = site_lon,
      lat = 0,
      address = "solar_time_reference",
      stringsAsFactors = FALSE
    )
    raw_solar <- SunCalcMeeus::solar_time(
      time = date_time[idx],
      geocode = geocode,
      unit.out = "datetime"
    )

    # `SunCalcMeeus::solar_time()` returns the solar time wrapped onto the
    # same calendar day as the UTC input (i.e., it adds or removes 24 h to
    # keep the clock reading within [00:00, 24:00) on the input date). Undo
    # that wrap so the returned timestamp lies on the correct solar
    # calendar day. The natural offset is bounded by abs(longitude)/15
    # hours, so normalising to [-12 h, +12 h] is sufficient.
    offset <- as.numeric(raw_solar) - as.numeric(date_time[idx])
    offset <- ((offset + 43200) %% 86400) - 43200
    out[idx] <- date_time[idx] + offset
  }

  out
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

  check_site_coord(longitude, length(solar_datetime))

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
