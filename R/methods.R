#' Apply imputation method to a numeric vector
#' @param x Numeric vector
#' @param method Imputation method
#' @param maxgap Maximum gap size
#' @return Imputed vector
#' @keywords internal
#' @import imputeTS
apply_imputation_method <- function(x, method, maxgap = Inf) {
  suppressMessages(
    suppressWarnings(
      switch(method,
        "kalman" = imputeTS::na_kalman(x, model = "auto.arima", maxgap = maxgap),
        "ma" = imputeTS::na_ma(x, k = 4, maxgap = maxgap),
        "linear" = imputeTS::na_interpolation(x, maxgap = maxgap, option = "linear"),
        "spline" = imputeTS::na_interpolation(x, maxgap = maxgap, option = "spline"),
        stop("Invalid method")
      )
    )
  )
}
