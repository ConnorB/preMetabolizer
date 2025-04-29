#' Calculate Coefficient of Variation
#'
#' @description
#' Computes the coefficient of variation (CV), defined as the ratio of the standard deviation
#' to the mean. Handles NA values, zero-mean cases, and provides percentage formatting.
#'
#' @param x A numeric vector for which to compute CV.
#' @param na.rm Logical indicating whether to remove NA values (default: TRUE).
#' @param as_percent Logical indicating whether to return result as percentage (default: TRUE).
#' @param robust Logical indicating whether to use median/MAD instead of mean/SD for robust CV (default: FALSE).
#'
#' @return The CV as numeric (percentage if `as_percent = TRUE`). Returns NA with warning for:
#' - Non-numeric input
#' - Zero-length input
#' - All-NA input (when na.rm = TRUE)
#' - Zero-mean input (for non-robust version)
#'
#' @examples
#' calc_cv(c(10, 20, 30, 40, 50))  # 47.14045 (percentage)
#' calc_cv(c(10, 20, 30, 40, 50), as_percent = FALSE)  # 0.4714045
#' calc_cv(c(10, 20, NA, 40, 50))  # NA removed by default
#' calc_cv(c(10, 20, NA, 40, 50), na.rm = FALSE)  # NA
#' calc_cv(numeric(0))  # NA with warning
#' calc_cv(c(0, 0, 0))  # NA with warning (zero mean)
#' calc_cv(c(0, 0, 0), robust = TRUE)  # Computes robust CV
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
    warning(if (robust) "Median is zero" else "Mean is zero", ", CV cannot be calculated")
    return(NA_real_)
  }

  cv_value <- dispersion / center

  if (as_percent) {
    cv_value <- cv_value * 100
  }

  cv_value
}
