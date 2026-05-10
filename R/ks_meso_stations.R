#' Get Kansas Mesonet station information
#'
#' Fetches metadata about Kansas Mesonet stations, including location and
#' network details.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A data frame containing station metadata.
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
#' stations <- ks_meso_stations()
#' subset(stations, grepl("Konza", StationName))
#' }
#'
#' @export
ks_meso_stations <- function() {
  url <- "http://mesonet.k-state.edu/rest/stationnames/"
  col_names <- c(
    "StationName",
    "County",
    "Latitude",
    "Longitude",
    "Elevation_m",
    "Network",
    "Abbreviation",
    "OperatorName",
    "FW13"
  )
  tryCatch(
    {
      response <- httr2::request(url) |> httr2::req_perform()
      content <- httr2::resp_body_string(response)
      station_names <- readr::read_csv(I(content), show_col_types = FALSE)
      if (ncol(station_names) == length(col_names)) {
        names(station_names) <- col_names
      }
      station_names
    },
    error = function(e) {
      stop(sprintf("Error fetching station names: %s", e$message))
    }
  )
}
