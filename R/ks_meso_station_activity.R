#' Get Kansas Mesonet station activity
#'
#' Retrieves activity data for Kansas Mesonet stations, including observation
#' intervals and data spans.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A data frame with station activity details, including start and end observation times.
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
#' subset(activity, station == "Konza Prairie")
#' }
#'
#' @export
ks_meso_station_activity <- function() {
  # Define the URL
  url <- "http://mesonet.k-state.edu/rest/stationactive/"

  tryCatch(
    {
      # Fetch data with retry capability
      response <- httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
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
          station = .data$STATION,
          interval = interval_map[as.character(.data$OBS_INTERVAL)],
          interval_seconds = .data$OBS_INTERVAL,
          first_observation = as.POSIXct(
            .data$START,
            tz = ks_meso_tz()
          ),
          last_observation = as.POSIXct(
            .data$END,
            tz = ks_meso_tz()
          ),
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
        dplyr::arrange(.data$station, .data$interval_seconds)

      # Add class for potential method dispatch
      class(station_activity) <- c("ks_meso_station_activity", "data.frame")

      return(station_activity)
    },
    error = function(e) {
      stop(sprintf("Error fetching station activity data: %s", e$message))
    }
  )
}
