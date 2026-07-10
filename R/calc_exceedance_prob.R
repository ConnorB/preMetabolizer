#' Calculate exceedance probabilities
#'
#' Calculates exceedance probabilities for a numeric vector, such as
#' stream discharge, using a plotting-position formula. Larger values
#' are assigned lower exceedance probabilities.
#'
#' @param values Numeric vector of values, such as stream discharge.
#' @param remove_zeros Logical. If `TRUE`, zero and negative values are
#'   excluded from the ranking and returned as `NA`. Defaults to `FALSE`.
#' @param alpha Numeric plotting-position constant between 0 and 0.5. Defaults
#'   to 0, corresponding to the Weibull plotting position. Common alternatives
#'   include 0.375 (Blom), 0.4 (Cunnane), 0.44 (Gringorten), and 0.5 (Hazen).
#' @param rm.zero `r lifecycle::badge("deprecated")` Use `remove_zeros`
#'   instead.
#'
#' @return A numeric vector of exceedance probabilities with the same length
#'   and order as `values`. Missing and infinite input values are returned as
#'   `NA`.
#'
#' @details
#' Exceedance probabilities are calculated using the general plotting-position
#' formula:
#' \deqn{P = \frac{\mathrm{rank} - \alpha}{n + 1 - 2\alpha}}
#' where rank is the descending rank of each value, n is the number of
#' ranked observations, and alpha is the plotting-position constant.
#'
#' Tied values are assigned the average of the ranks they would otherwise
#' occupy, so identical values receive identical exceedance probabilities.
#'
#' @references
#' Cunnane, C. (1978). Unbiased plotting positions --- A review.
#' *Journal of Hydrology*, 37(3--4), 205--222.
#' \doi{10.1016/0022-1694(78)90017-3}
#'
#' @examples
#' flow_data <- c(10, 5, 0, 15, 8, NA, 0, 20)
#'
#' calc_exceedance_prob(flow_data)
#' calc_exceedance_prob(flow_data, remove_zeros = TRUE)
#' calc_exceedance_prob(flow_data, alpha = 0.4)
#'
#' @export
calc_exceedance_prob <- function(
  values,
  remove_zeros = FALSE,
  alpha = 0,
  rm.zero = lifecycle::deprecated()
) {
  if (lifecycle::is_present(rm.zero)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "calc_exceedance_prob(rm.zero)",
      "calc_exceedance_prob(remove_zeros)"
    )
    remove_zeros <- rm.zero
  }

  # Input type and validity checks
  if (!is.numeric(values)) {
    cli::cli_abort("{.arg values} must be a numeric vector.")
  }

  check_bool(remove_zeros)
  check_alpha(alpha)

  if (length(values) == 0) {
    cli::cli_warn("{.arg values} is empty. Returning an empty numeric vector.")
    return(numeric(0))
  }

  # Treat infinite values as missing
  values[is.infinite(values)] <- NA

  # Store the original length and indices of valid (non-NA) values
  original_length <- length(values)
  valid_indices <- which(!is.na(values))
  non_zero_indices <- which(values > 0 & !is.na(values))

  # Remove NA values for ranking
  values_no_na <- values[valid_indices]

  if (remove_zeros) {
    # Remove zeros
    values_no_na <- values_no_na[values_no_na > 0]
    if (length(values_no_na) == 0) {
      cli::cli_warn(
        "All non-missing values in {.arg values} were zero. Returning `NA` values."
      )
      return(rep(NA, original_length))
    }
  }

  # Calculate the rank of each value (highest to lowest)
  ranked_values <- rank(-values_no_na, ties.method = "average")

  # Calculate the number of observations
  n <- length(values_no_na)

  # Calculate the exceedance probability using the plotting position formula
  exceedance_probability <- (ranked_values - alpha) / (n + 1 - 2 * alpha)

  # Create full-length vector with NAs where appropriate
  full_exceedance_prob <- rep(NA, original_length)
  if (remove_zeros) {
    full_exceedance_prob[non_zero_indices] <- exceedance_probability
  } else {
    full_exceedance_prob[valid_indices] <- exceedance_probability
  }

  return(full_exceedance_prob)
}

#' Calculate exceedance probabilities with C++
#'
#' Calculates exceedance probabilities using the same plotting position
#' method and return shape as [calc_exceedance_prob()], but delegates ranking to
#' a C++ implementation.
#'
#' @inheritParams calc_exceedance_prob
#'
#' @return A numeric vector of exceedance probabilities. If
#'   `remove_zeros = TRUE`, the returned vector has the same length as the
#'   input with `NA` at positions of zero or negative values.
#'
#' @seealso [calc_exceedance_prob()]
#'
#' @examples
#' rcpp_calc_exceedance_prob(c(10, 5, 0, 15, 8, NA, 0, 20))
#'
#' @export
rcpp_calc_exceedance_prob <- function(
  values,
  remove_zeros = FALSE,
  alpha = 0,
  rm.zero = lifecycle::deprecated()
) {
  if (lifecycle::is_present(rm.zero)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "rcpp_calc_exceedance_prob(rm.zero)",
      "rcpp_calc_exceedance_prob(remove_zeros)"
    )
    remove_zeros <- rm.zero
  }

  if (!is.numeric(values)) {
    cli::cli_abort("{.arg values} must be a numeric vector.")
  }

  check_bool(remove_zeros)
  check_alpha(alpha)

  if (length(values) == 0) {
    cli::cli_warn("{.arg values} is empty. Returning an empty numeric vector.")
    return(numeric(0))
  }

  # Treat infinite values as missing
  values[is.infinite(values)] <- NA

  if (remove_zeros && length(values[values > 0 & !is.na(values)]) == 0) {
    cli::cli_warn(
      "All non-missing values in {.arg values} were zero. Returning `NA` values."
    )
    return(rep(NA, length(values)))
  }

  cpp_calc_exceedance_prob(values, remove_zeros, alpha)
}

check_alpha <- function(alpha, call = parent.frame()) {
  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha < 0 ||
      alpha > 0.5
  ) {
    cli::cli_abort(
      "{.arg alpha} must be a single number between 0 and 0.5.",
      call = call
    )
  }
}
