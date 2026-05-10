#' Get most recent Kansas Mesonet data timestamp
#'
#' Retrieves the timestamp of the most recently ingested data for each station
#' for a given observation interval. The returned data includes the `"--all--"`
#' row reported by Kansas Mesonet.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param interval Data interval. Must be one of `"hour"`, `"5min"`, or
#'   `"day"`.
#'
#' @return A data frame with station names and most recent observation times.
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
#' @examples
#' \dontrun{
#' ks_meso_most_recent(interval = "hour")
#' }
#'
#' @export
ks_meso_most_recent <- function(interval) {
  interval <- rlang::arg_match(interval, c("hour", "5min", "day"))

  tryCatch(
    {
      response <- httr2::request(
        "http://mesonet.k-state.edu/rest/mostrecent"
      ) |>
        httr2::req_url_query(int = interval) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
      content <- httr2::resp_body_string(response)

      read_ks_meso_csv(I(content)) |>
        dplyr::transmute(
          station = .data$STATION,
          timestamp = .data$TIMESTAMP
        )
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to fetch most recent Kansas Mesonet data.",
        parent = e
      )
    }
  )
}
