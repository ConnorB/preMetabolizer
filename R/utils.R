#' Convert Barometric Pressure to Atmospheres
#'
#' This helper function converts barometric pressure from various units
#' to atmospheres (\[atm\]). It is intended for internal use within the package.
#'
#' @param pressure Numeric. Barometric pressure value(s) to be converted.
#' @param units Character. Units of the input barometric pressure. Accepted values are
#'   `"atm"`, `"hPa"`, `"mbar"`, `"kPa"`, `"Torr"`, `"psi"`, and `"bar"`.
#'
#' @return Numeric. Barometric pressure in atmospheres (\[atm\]).
#'
#' @keywords internal
#' @export
convert_pressure_to_atm <- function(pressure, units) {
  if (units == "atm") {
    pressure_atm <- pressure
  } else if (units == "hPa" || units == "mbar") {
    pressure_atm <- pressure * 0.00098692316931427
  } else if (units == "kPa") {
    pressure_atm <- pressure * 0.0098692316931427
  } else if (units == "Torr") {
    pressure_atm <- pressure / 760
  } else if (units == "psi") {
    pressure_atm <- pressure * 0.0680459639
  } else if (units == "bar") {
    pressure_atm <- pressure * 0.98692316931427
  } else {
    stop(
      "Please report barometric pressure in units of `atm`, `hPa`, `mbar`, `kPa`, `Torr`, `psi`, or `bar`."
    )
  }

  return(pressure_atm)
}


#' Get the Last Modified Time of a Remote File (Internal)
#'
#' This internal function retrieves the "Last-Modified" timestamp of a remote file by sending an HTTP HEAD request to the given URL. The result is memoised to cache results for repeated calls with the same URL.
#'
#' @param url A character string specifying the URL of the remote file.
#'
#' @return A POSIXlt object representing the last modified timestamp of the remote file, or `NULL` if the information is unavailable or an error occurs.
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
#' @keywords internal
get_remote_mtime <- memoise::memoise(function(url) {
  tryCatch(
    {
      resp <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_perform()
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
