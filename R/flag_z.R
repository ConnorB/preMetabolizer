#' Flag Outliers Using Robust Z-Scores
#'
#' Identifies potential outliers in a numeric vector based on a moving window
#' robust Z-score approach. The robust Z-score is computed using a
#' biweight scale estimate centered on the median.
#'
#' @param x A numeric vector to be checked for outliers.
#' @param width An odd integer specifying the width of the moving window (default: 5).
#' @param threshold A numeric threshold for the absolute Z-score above which a value is flagged (default: 3.0).
#' @param return_z Logical; if `TRUE`, returns both Z-scores and flags. If `FALSE`, returns only flags (default: `FALSE`).
#'
#' @details
#' For each value in `x`, a window of length `width` centered on that value
#' is extracted. The function:
#' - Computes the median of the window.
#' - Calculates residuals from the median.
#' - Estimates a robust scale using Tukey’s biweight estimator based on MAD.
#' - Computes a Z-score as \eqn{(x_i - \text{median}) / \text{scale}}.
#'
#' If the absolute value of the Z-score exceeds `threshold`, the value is flagged with `"Z"`.
#'
#' NA values are ignored in the window statistics but retained in output positions.
#'
#' @return If `return_z = TRUE`, a list with:
#' \describe{
#'   \item{z}{A numeric vector of robust Z-scores (with `NA` where not computable).}
#'   \item{flag}{A character vector of same length as `x`, with `"Z"` where an outlier is detected, and `NA` otherwise.}
#' }
#' If `return_z = FALSE`, only the `flag` vector is returned.
#'
#' @examples
#' x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
#' flag_z(x)
#' flag_z(x, return_z = TRUE)
#'
#' @export
#' @useDynLib preMetabolizer, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @seealso \code{\link[stats]{mad}}, \code{\link[stats]{median}}
flag_z <- function(x, width = 5, threshold = 3.0, return_z = FALSE) {
  .Call(`_preMetabolizer_flag_z`, x, width, threshold, return_z)
}
