#' Convert Stream Discharge from Cubic Feet per Second to Various Units
#'
#' This function converts stream discharge measurements from cubic feet per second (cfs)
#' to cubic meters per second (cms) and liters per second (l/s).
#'
#' @param cfs A numeric vector of stream discharge in cubic feet per second.
#' @param to A character string specifying the target unit. Acceptable values are
#'   "cms" (cubic meters per second) or "lps" (liters per second). Default is "cms".
#'
#' @return A numeric vector of stream discharge in the specified unit.
#' @export
#'
#' @examples
#' convert_cfs(cfs = 14.32, to = "cms")  # Converts 14.32 cfs to cms
#' convert_cfs(cfs = 14.32, to = "lps")  # Converts 14.32 cfs to lps
#' convert_cfs(cfs = c(10, 20, 30), to = "cms")  # Converts multiple values to cms
convert_cfs <- function(cfs, to = "cms") {
  # Ensure input is a numeric vector, allowing NA values
  if (!is.numeric(cfs) && !all(is.na(cfs))) {
    stop("Input must be a numeric vector.")
  }

  # Ensure valid target unit
  if (!to %in% c("cms", "lps")) {
    stop('to must be either "cms" or "lps".')
  }

  # Conversion factors
  cfs_to_cms_factor <- 0.028316847  # Cubic feet per second to cubic meters per second
  cfs_to_lps_factor <- cfs_to_cms_factor * 1000  # Cubic feet per second to liters per second

  # Perform conversion
  if (to == "cms") {
    Q_converted <- cfs * cfs_to_cms_factor
  } else if (to == "lps") {
    Q_converted <- cfs * cfs_to_lps_factor
  }

  return(Q_converted)
}

