#' Fast, easy, and even time steps
#' @description easily get even and matching timesteps for a dataframe
#'
#' @param loggerData logger dataframe with a DateTime_UTC column
#'
#' @return a dataframe
#' @import lubridate
#'
find_mode <- function(loggerData) {
  u <- unique(loggerData$DateTime_UTC)
  tab <- tabulate(match(loggerData$DateTime_UTC, u))
  u[tab == max(tab)]
}

even_timesteps <- function(loggerData) {
  setDF(loggerData)
  measInt <- as.period(find_mode(diff(loggerData$DateTime_UTC)), unit = "minute")
  intSeconds <- as.numeric(measInt)
  startDate <- ceiling_date(min(loggerData$DateTime_UTC), measInt)
  endDate <- floor_date(max(loggerData$DateTime_UTC), measInt)
  temp.seq <- seq(startDate, endDate, by = intSeconds)
  temp.fill <- data.frame(DateTime_UTC = temp.seq)
  loggerData <- merge(loggerData, temp.fill, by = c("DateTime_UTC"), all = T)
}
