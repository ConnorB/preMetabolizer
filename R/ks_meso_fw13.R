#' Get Kansas Mesonet FW13 Data
#'
#' Retrieves fire weather data in FW13 format for one station and date range.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param station Station name as a character string.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` or
#'   `YYYYMMDD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD`
#'   format.
#'
#' @return A character vector containing FW13 records.
#'
#' @details
#' Kansas Mesonet data are preliminary and subject to revision. Cite the Kansas
#' Mesonet when sharing, publishing, or otherwise disseminating data accessed
#' with this function. A suggested citation format is: Kansas Mesonet, year:
#' webpage title. Accessed date, webpage URL. Review the Kansas Mesonet data
#' usage policy before automated use; automated page scraping or data ingesting
#' without written consent is not permitted.
#'
#' @references
#' Kansas Mesonet data usage policy:
#' \url{https://mesonet.k-state.edu/about/usage/}
#'
#' @export
ks_meso_fw13 <- function(station, start_date, end_date) {
  if (!is.character(station) || length(station) != 1 || is.na(station)) {
    stop("station must be a single character string")
  }

  dates <- validate_ks_meso_dates(start_date, end_date)

  tryCatch(
    {
      response <- httr2::request("http://mesonet.k-state.edu/rest/fw13") |>
        httr2::req_url_query(
          stn = station,
          t_start = format(dates$start, "%Y%m%d"),
          t_end = format(dates$end, "%Y%m%d")
        ) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()

      strsplit(httr2::resp_body_string(response), "\n", fixed = TRUE)[[1]]
    },
    error = function(e) {
      stop(sprintf("Error fetching FW13 Mesonet data: %s", e$message))
    }
  )
}
