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
