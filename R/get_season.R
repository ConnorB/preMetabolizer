#' Determine the Season from a Date
#'
#' This function determines the meteorological season (Winter, Spring, Summer, or Fall) based on the input date.
#' It handles invalid inputs and provides clear error messages.
#'
#' @param date A character string or Date object. If a character string, it should be in a format that can be coerced to a Date.
#' @return A character string indicating the season: "Winter", "Spring", "Summer", or "Fall".
#' @importFrom dplyr case_when
#' @importFrom lubridate day month
#' @examples
#' get_season("2024-12-25")
#' get_season(as.Date("2024-07-04"))
#' @export
get_season <- function(date) {

  # Handle NULL and NA
  if (is.null(date)) {
    stop("Error: Input is NULL. Please provide a valid date.")
  }
  if (length(date) == 0 || is.na(date)) {
    stop("Error: Input is NA. Please provide a valid date.")
  }
  # Convert input to Date if necessary
  date <- tryCatch(base::as.Date(date),
                   error = function(e) stop("Error: Input is not a valid date or cannot be coerced to a Date."))

  # Extract month and day
  month <- date |> month()
  day <- date |> day()

  # Validate month and day
  if (is.na(month) || is.na(day)) {
    stop("Error: Unable to extract month or day from the input date.")
  }

  # Determine the season
  season <- case_when(
    (month == 12 & day >= 21) | (month %in% c(1, 2)) | (month == 3 & day < 20) ~ "Winter",
    (month == 3 & day >= 20) | (month %in% c(4, 5)) | (month == 6 & day < 21) ~ "Spring",
    (month == 6 & day >= 21) | (month %in% c(7, 8)) | (month == 9 & day < 23) ~ "Summer",
    (month == 9 & day >= 23) | (month %in% c(10, 11)) | (month == 12 & day < 21) ~ "Fall",
    TRUE ~ NA_character_  # Return NA if no condition matches
  )


  return(season)
}
