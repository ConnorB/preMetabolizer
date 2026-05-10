#' Determine the season from a date
#'
#' Classifies dates into astronomical seasons: Winter, Spring, Summer, or Fall.
#'
#' @param date Character, Date, or date-time vector coercible with
#'   [base::as.Date()].
#'
#' @return Character vector of season names with one value per input date.
#'
#' @details
#' Seasons use fixed northern-hemisphere transition dates: Spring begins
#' March 20, Summer begins June 21, Fall begins September 23, and Winter begins
#' December 21.
#'
#' @examples
#' get_season("2024-12-25")
#'
#' get_season(as.Date(c(
#'   "2024-01-15",
#'   "2024-04-15",
#'   "2024-07-15",
#'   "2024-10-15"
#' )))
#'
#' @export
get_season <- function(date) {
  # Convert input to Date if necessary
  date <- tryCatch(base::as.Date(date), error = function(e) {
    stop("Error: Input is not a valid date or cannot be coerced to a Date.")
  })

  # Extract month and day
  month <- lubridate::month(date)
  day <- lubridate::day(date)

  # Determine the season
  season <- dplyr::case_when(
    (month == 12 & day >= 21) |
      (month %in% c(1, 2)) |
      (month == 3 & day < 20) ~ "Winter",
    (month == 3 & day >= 20) |
      (month %in% c(4, 5)) |
      (month == 6 & day < 21) ~ "Spring",
    (month == 6 & day >= 21) |
      (month %in% c(7, 8)) |
      (month == 9 & day < 23) ~ "Summer",
    (month == 9 & day >= 23) |
      (month %in% c(10, 11)) |
      (month == 12 & day < 21) ~ "Fall",
    TRUE ~ NA_character_ # Return NA if no condition matches
  )

  return(season)
}
