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
#' @source Kansas Mesonet, \url{http://mesonet.k-state.edu/rest/variables/}
"ks_mesonet_vars"
