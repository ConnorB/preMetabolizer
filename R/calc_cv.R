#' Calculate the coefficient of variation
#'
#' Computes the coefficient of variation (CV), a unitless measure of relative
#' variability. By default, the result is returned as a percentage.
#'
#' @param x Numeric vector.
#' @param na.rm Logical. If `TRUE`, remove missing values before calculation.
#'   Defaults to `TRUE`.
#' @param as_percent Logical. If `TRUE`, multiply the CV by 100. Defaults to
#'   `TRUE`.
#' @param robust Logical. If `TRUE`, use median and MAD instead of mean and
#'   standard deviation. Defaults to `FALSE`.
#'
#' @return A single numeric CV value, or `NA_real_` when the input is
#'   non-numeric, empty, all missing after `NA` removal, or centered on zero.
#'   Problematic inputs also produce a warning.
#'
#' @examples
#' discharge <- c(0.12, 0.18, 0.15, 1.4, 0.09)
#'
#' calc_cv(discharge)
#' calc_cv(discharge, as_percent = FALSE)
#' calc_cv(discharge, robust = TRUE)
#'
#' @export
calc_cv <- function(x, na.rm = TRUE, as_percent = TRUE, robust = FALSE) {
  if (!is.numeric(x)) {
    warning("Input must be numeric")
    return(NA_real_)
  }

  if (length(x) == 0) {
    warning("Empty input vector")
    return(NA_real_)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      warning("No non-NA values in input")
      return(NA_real_)
    }
  } else if (any(is.na(x))) {
    return(NA_real_)
  }

  if (robust) {
    center <- stats::median(x)
    dispersion <- stats::mad(x, constant = 1)
  } else {
    center <- mean(x)
    dispersion <- stats::sd(x)
  }

  if (abs(center) < .Machine$double.eps^0.5) {
    warning(
      if (robust) "Median is zero" else "Mean is zero",
      ", CV cannot be calculated"
    )
    return(NA_real_)
  }

  cv_value <- dispersion / center

  if (as_percent) {
    cv_value <- cv_value * 100
  }

  cv_value
}
