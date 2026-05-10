#' Read cached Kansas Mesonet data
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
#' @examples
#' \dontrun{
#' konza_hourly <- read_ks_meso(
#'   station = "Konza Prairie",
#'   start_date = "2024-06-01",
#'   end_date = "2024-06-07",
#'   interval = "hour"
#' )
#' }
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
    cli::cli_abort("{.arg station} must be provided as a character vector.")
  }

  if (
    missing(start_date) ||
      missing(end_date) ||
      !inherits(lubridate::ymd(start_date), "Date") ||
      !inherits(lubridate::ymd(end_date), "Date")
  ) {
    cli::cli_abort(
      "{.arg start_date} and {.arg end_date} must be valid date strings in YYYY-MM-DD format."
    )
  }

  if (missing(interval) || !is.character(interval)) {
    cli::cli_abort("{.arg interval} must be provided as a character string.")
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
    cli::cli_abort(
      c(
        "Cached Kansas Mesonet file does not exist.",
        "i" = "Expected file: {.path {output_file}}."
      )
    )
  }

  read_ks_meso_csv(output_file)
}
