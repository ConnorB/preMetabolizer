#' Get Kansas Mesonet station activity
#'
#' Retrieves activity data for Kansas Mesonet stations, including observation
#' intervals and data spans.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A tibble with station activity details, including station names in
#'   `station_name` and start and end observation times.
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
#' activity <- ks_meso_station_activity()
#' subset(activity, station_name == "Konza Prairie")
#' }
#'
#' @export
ks_meso_station_activity <- function() {
  tryCatch(
    {
      response <- ks_meso_request("stationactive") |> http_req_perform()
      content <- httr2::resp_body_string(response)

      interval_map <- c(
        "300" = "5min",
        "3600" = "hour",
        "86400" = "day"
      )

      station_activity <- readr::read_csv(
        I(content),
        col_types = readr::cols(
          START = readr::col_character(),
          END = readr::col_character(),
          .default = readr::col_guess()
        ),
        show_col_types = FALSE
      ) |>
        dplyr::transmute(
          station_name = .data$STATION,
          interval = unname(interval_map[as.character(.data$OBS_INTERVAL)]),
          interval_seconds = .data$OBS_INTERVAL,
          first_observation = parse_ks_meso_timestamp(.data$START),
          last_observation = parse_ks_meso_timestamp(.data$END),
          data_span_days = as.numeric(
            difftime(
              .data$last_observation,
              .data$first_observation,
              units = "days"
            )
          ),
          is_current = as.numeric(
            difftime(Sys.time(), .data$last_observation, units = "hours")
          ) <
            24
        ) |>
        dplyr::arrange(.data$station_name, .data$interval_seconds)

      class(station_activity) <- c(
        "ks_meso_station_activity",
        "tbl_df",
        "tbl",
        "data.frame"
      )

      station_activity
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to fetch Kansas Mesonet station activity data.",
        parent = e
      )
    }
  )
}
