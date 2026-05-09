#' Calculate histogram bin width
#'
#' Computes a suggested bin width for a histogram using one of several
#' classical rules. `NA` values are silently removed before computation.
#'
#' @param x Numeric vector of observations. Must contain at least two
#'   distinct, finite, non-`NA` values.
#' @param method Single character string specifying the binning rule. One of:
#'   \describe{
#'     \item{`"auto"`}{Minimum of Freedman-Diaconis and Sturges (default).
#'       Tends to work well across a wide range of distributions.}
#'     \item{`"sturges"`}{Sturges' rule: \eqn{h = R / (\log_2 n + 1)}.
#'       Assumes approximate normality; tends to undersmooth for large
#'       \eqn{n}.}
#'     \item{`"fd"`}{Freedman-Diaconis: \eqn{h = 2 \cdot \mathrm{IQR}(x)
#'       \cdot n^{-1/3}}. Robust to outliers. Falls back to Sturges when
#'       \eqn{\mathrm{IQR}(x) = 0}.}
#'     \item{`"sqrt"`}{Square-root rule: \eqn{h = R / \sqrt{n}}. Simple
#'       heuristic used by some spreadsheet applications.}
#'     \item{`"rice"`}{Rice rule: \eqn{h = R / (2 n^{1/3})}. Similar to
#'       Sturges but grows more slowly.}
#'     \item{`"scott"`}{Scott's rule: \eqn{h = 3.49 \hat\sigma n^{-1/3}}.
#'       Optimal for normal data in the sense of minimising mean integrated
#'       squared error.}
#'     \item{`"doane"`}{Doane's rule. Extends Sturges to account for
#'       skewness; better suited to non-normal distributions. Requires
#'       \eqn{n \ge 3}.}
#'   }
#'
#' @return A single positive numeric value giving the bin width.
#'
#' @details
#' In all formulae, \eqn{R} denotes the range (`diff(range(x))`) and
#' \eqn{n} the number of non-`NA` observations.
#'
#' The `"fd"` fallback to Sturges when `IQR(x) == 0` avoids a zero-width
#' bin, which can occur with heavily discrete or zero-inflated data.
#'
#' @section Errors:
#' The function aborts with an informative message when:
#' \itemize{
#'   \item `x` is not a numeric vector.
#'   \item `method` is not a single character string.
#'   \item `x` contains no non-`NA` values, or only one.
#'   \item All non-`NA` values in `x` are identical (zero range).
#'   \item `method = "doane"` is requested with fewer than 3 observations
#'         (the skewness standard error is undefined for \eqn{n < 3}).
#'   \item An unrecognised `method` string is supplied.
#' }
#'
#' @examples
#' # Continuous data — Freedman-Diaconis
#' calc_bin_width(rnorm(200), "fd")
#'
#' # Skewed data — Doane corrects for skewness
#' calc_bin_width(rexp(200), "doane")
#'
#' # Discrete / zero-inflated data — FD falls back to Sturges
#' calc_bin_width(c(rep(0, 50), rep(1, 50)), "fd")
#'
#' # NA values are silently dropped
#' calc_bin_width(c(NA, rnorm(100), NA), "scott")
#'
#' # Compare all methods on the same data
#' x <- rnorm(500)
#' methods <- c("auto", "sturges", "fd", "sqrt", "rice", "scott", "doane")
#' vapply(methods, \(m) calc_bin_width(x, m), numeric(1))
#'
#' # Error conditions
#' try(calc_bin_width(letters))               # non-numeric x
#' try(calc_bin_width(c(1, NA, NA)))          # only one non-NA value
#' try(calc_bin_width(rep(5, 50)))            # zero range
#' try(calc_bin_width(rnorm(200), "badrule")) # unknown method
#' try(calc_bin_width(c(1, 2), "doane"))      # doane needs n >= 3
#'
#' @export
calc_bin_width <- function(x, method = "auto") {
  if (!is.numeric(x)) {
    cli::cli_abort(c(
      "{.arg x} must be a numeric vector.",
      "x" = "Got an object of class {.cls {class(x)}}."
    ))
  }
  if (!is.character(method) || length(method) != 1L) {
    cli::cli_abort(c(
      "{.arg method} must be a single character string.",
      "x" = "Got {.obj_type_friendly {method}} of length {length(method)}."
    ))
  }

  x <- x[!is.na(x)]
  n <- length(x)

  if (n == 0L) {
    cli::cli_abort(c(
      "{.arg x} contains no non-{.val NA} values.",
      "i" = "Provide at least two non-{.val NA} observations."
    ))
  }
  if (n == 1L) {
    cli::cli_abort(c(
      "{.arg x} contains only one non-{.val NA} value.",
      "i" = "At least two observations are required to compute a bin width."
    ))
  }

  rng <- diff(range(x))

  if (rng == 0) {
    cli::cli_abort(c(
      "{.arg x} has zero range: all non-{.val NA} values are identical.",
      "i" = "Bin width is undefined when all observations are the same."
    ))
  }

  if (method == "doane" && n < 3L) {
    cli::cli_abort(c(
      "Doane's rule requires at least 3 observations.",
      "x" = "{.arg x} has only {n} non-{.val NA} value{?s} after removing {.val NA}s.",
      "i" = "Use a different {.arg method} or supply more data."
    ))
  }

  sturges_width <- rng / (log2(n) + 1)
  fd_raw <- 2 * stats::IQR(x) * n^(-1 / 3)
  fd_width <- if (fd_raw == 0) sturges_width else fd_raw

  switch(
    method,
    auto = min(fd_width, sturges_width),
    sturges = sturges_width,
    fd = fd_width,
    sqrt = rng / sqrt(n),
    rice = rng / (2 * n^(1 / 3)),
    scott = 3.49 * stats::sd(x) * n^(-1 / 3),
    doane = {
      g1 <- mean((x - mean(x))^3) / stats::sd(x)^3
      sg1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
      rng / (1 + log2(n) + log2(1 + abs(g1) / sg1))
    },
    cli::cli_abort(c(
      "Invalid {.arg method} argument.",
      "x" = "Method {.val {method}} is not supported.",
      "i" = "Allowed methods: {.val {c('auto', 'sturges', 'fd', 'sqrt', 'rice', 'scott', 'doane')}}"
    ))
  )
}
