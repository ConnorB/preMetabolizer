#' Read Cached Kansas Mesonet Data
#'
#' Reads previously downloaded and cached Kansas Mesonet data.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param station Station name as a character string.
#' @param start_date Start date for the data in `YYYY-MM-DD` format.
#' @param end_date End date for the data in `YYYY-MM-DD` format.
#' @param interval Data interval. Must be one of `'hour'`, `'5min'`, or `'day'`.
#' @param output_dir Directory where the cached data is stored. Defaults to the cache path.
#'
#' @return A data frame containing the requested Mesonet data.
#'
#' @details
#' Kansas Mesonet data are preliminary and subject to revision. Cite the Kansas
#' Mesonet when sharing, publishing, or otherwise disseminating data read with
#' this function. A suggested citation format is: Kansas Mesonet, year: webpage
#' title. Accessed date, webpage URL. Review the Kansas Mesonet data usage
#' policy before automated use; automated page scraping or data ingesting
#' without written consent is not permitted.
#'
#' @references
#' Kansas Mesonet data usage policy:
#' \url{https://mesonet.k-state.edu/about/usage/}
#'
#' @export
read_ks_meso <- function(
  station,
  start_date,
  end_date,
  interval,
  output_dir = NULL
) {
  if (missing(station) || !is.character(station)) {
    stop("station must be provided as a character string")
  }

  if (
    missing(start_date) ||
      missing(end_date) ||
      !inherits(lubridate::ymd(start_date), "Date") ||
      !inherits(lubridate::ymd(end_date), "Date")
  ) {
    stop(
      "start_date and end_date must be provided as valid date strings (YYYY-MM-DD)"
    )
  }

  if (missing(interval) || !is.character(interval)) {
    stop("interval must be provided as a character string")
  }

  if (is.null(output_dir)) {
    output_dir <- mesonet_cache()
  }

  # Create the expected file path
  output_file <- file.path(
    output_dir,
    sprintf(
      "Mesonet_%s_%s_%s_to_%s.csv",
      station,
      interval,
      start_date,
      end_date
    )
  )

  if (!file.exists(output_file)) {
    stop(sprintf(
      "Cached file for station %s, interval %s, and date range %s to %s does not exist.",
      station,
      interval,
      start_date,
      end_date
    ))
  }

  read_ks_meso_csv(output_file)
}
