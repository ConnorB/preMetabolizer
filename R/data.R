#' Kansas Mesonet Variables
#'
#' This dataset contains variable names, units, and descriptions as provided by the Kansas Mesonet.
#' These variables represent a wide range of meteorological, hydrological, and soil measurements.
#'
#' @format A tibble with 150 rows and 4 columns:
#' \describe{
#'   \item{Variable}{The original variable name as used by the Kansas Mesonet.}
#'   \item{CleanName}{A cleaned and standardized version of the variable name for easier use in R.}
#'   \item{Units}{The units of measurement for the variable (if applicable).}
#'   \item{Description}{A brief description of the variable and its purpose.}
#' }
#'
#' @source Kansas Mesonet, <http://mesonet.k-state.edu/rest/variables/>
"ks_mesonet_vars"

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
#' - **Elevation**: approximately 2,195 m (7,200 ft)
#' - **Location**: French Creek, Laramie, Wyoming, USA
#'
#' The two `sonde` values indicate a mid-study sensor swap on
#' September 1, 2012. For most analyses, filter to a single sonde or
#' treat the transition as a potential data break.
#'
#' @source Hall, R. O., Jr., Tank, J. L., Baker, M. A., Rosi-Marshall,
#'   E. J., & Hotchkiss, E. R. (2016). Metabolism, gas exchange, and
#'   carbon spiraling in rivers. *Ecosystems*, 19(1), 73--86.
#'   \doi{10.1890/14-0631.1}
"french_creek"
