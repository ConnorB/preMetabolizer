#' French Creek Stream Metabolism Data
#'
#' Dissolved oxygen and water temperature measured at 5-minute intervals on
#' French Creek near Laramie, Wyoming, USA during summer/fall 2012. Data
#' were collected as part of a stream metabolism study.
#'
#' @format A tibble with 10,883 rows and 4 columns:
#' \describe{
#'   \item{datetime}{POSIXct. Date and time in UTC (original timezone:
#'     MDT, UTC-6).}
#'   \item{sonde}{Character. Sensor identifier. `"REZN"` was deployed
#'     8/23/2012 through 9/1/2012 12:50 MDT; `"TOWN"` from
#'     9/1/2012 12:55 MDT through 9/30/2012. Approximately 15% of
#'     rows have `NA` in `temp_C` and `DO_mgL`.}
#'   \item{temp_C}{Numeric. Water temperature in degrees Celsius. Some
#'     anomalous negative values are present in the raw data and have
#'     not been filtered.}
#'   \item{DO_mgL}{Numeric. Dissolved oxygen concentration in mg/L.}
#' }
#'
#' @details
#' **Site information** (constant across all rows):
#' - **Latitude**: 41.33 N
#' - **Longitude**: -106.3 W
#' - **Stream depth**: 0.16 m
#' - **Elevation**: approximately 3,115 m (10220 ft)
#' - **Location**: French Creek, Laramie, Wyoming, USA
#'
#' The two `sonde` values indicate a mid-study sensor swap on
#' September 1, 2012. For most analyses, filter to a single sonde or
#' treat the transition as a potential data break.
#'
#' @source Hotchkiss, E. R., & Hall, R. O., Jr. (2015). Whole-stream
#'   13C tracer addition reveals distinct fates of newly fixed carbon.
#'   *Ecology*, 96, 403--416.
#'   \doi{10.1890/14-0631.1}
"french_creek"
#' Kings Creek daily water data
#'
#' Daily USGS water data for Kings Creek at monitoring location
#' `USGS-06879650`.
#'
#' The dataset includes daily observations for water year 2025,
#' from 2024-10-01 through 2025-09-30.
#'
#' @format A tibble with 716 rows and 5 variables:
#' \describe{
#'   \item{monitoring_location_id}{USGS monitoring location ID.}
#'   \item{time}{Date of observation.}
#'   \item{value}{Observed value. Units depend on `parameter_code`.}
#'   \item{parameter_code}{USGS parameter code: `00060` = discharge, `00065` = gage height, `00010` = water temperature.}
#'   \item{qualifier}{USGS data qualifier.}
#' }
#'
#' @source USGS Water Data API, retrieved with `dataRetrieval::read_waterdata_daily()`.
#'
"kings_discharge"
