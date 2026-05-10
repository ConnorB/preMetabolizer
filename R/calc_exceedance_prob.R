#' Calculate flow exceedance probabilities
#'
#' Calculates exceedance probabilities for a discharge vector with the Weibull
#' plotting-position formula. Higher flows receive lower exceedance
#' probabilities.
#'
#' @param flow Numeric vector of flow or discharge values.
#' @param rm.zero Logical. If `TRUE`, zero and negative values are omitted from
#'   the ranking and returned as `NA`. Defaults to `FALSE`.
#'
#' @return Numeric vector of exceedance probabilities with the same length as
#'   `flow`. Missing input values return `NA`.
#'
#' @details
#' The Weibull plotting position is:
#' \deqn{P = \frac{\mathrm{rank}}{n + 1}}
#' where `rank` is the descending rank of the flow value and `n` is the number
#' of ranked observations.
#'
#' @examples
#' flow_data <- c(10, 5, 0, 15, 8, NA, 0, 20)
#'
#' calc_exceedance_prob(flow_data)
#' calc_exceedance_prob(flow_data, rm.zero = TRUE)
#'
#' @export
calc_exceedance_prob <- function(flow, rm.zero = FALSE) {
  # Input type and validity checks
  if (!is.numeric(flow)) {
    stop("'flow' must be a numeric vector.")
  }

  if (any(is.infinite(flow))) {
    stop("'flow' contains infinite values. These are not allowed.")
  }

  if (!is.logical(rm.zero)) {
    stop("'rm.zero' must be a logical value (TRUE or FALSE).")
  }

  if (length(flow) == 0) {
    warning("'flow' is an empty vector. Returning an empty numeric vector.")
    return(numeric(0))
  }

  # Store the original length and indices of valid (non-NA) values
  original_length <- length(flow)
  valid_indices <- which(!is.na(flow))
  non_zero_indices <- which(flow > 0 & !is.na(flow))

  # Remove NA values for ranking
  flow_no_na <- flow[valid_indices]

  if (rm.zero) {
    # Remove zeros
    flow_no_na <- flow_no_na[flow_no_na > 0]
    if (length(flow_no_na) == 0) {
      warning(
        "All non-NA values in 'flow' were zero. Returning a vector of NAs with the original length."
      )
      return(rep(NA, original_length))
    }
  }

  # Calculate the rank of each flow value (highest to lowest)
  ranked_flow <- rank(-flow_no_na, ties.method = "average")

  # Calculate the number of observations
  n <- length(flow_no_na)

  # Calculate the exceedance probability using the Weibull plotting position
  exceedance_probability <- ranked_flow / (n + 1)

  # Create full-length vector with NAs where appropriate
  full_exceedance_prob <- rep(NA, original_length)
  if (rm.zero) {
    full_exceedance_prob[non_zero_indices] <- exceedance_probability
  } else {
    full_exceedance_prob[valid_indices] <- exceedance_probability
  }

  return(full_exceedance_prob)
}

#' Calculate flow exceedance probabilities with C++
#'
#' Calculates exceedance probabilities using the same Weibull plotting position
#' method and return shape as [calc_exceedance_prob()], but delegates ranking to
#' a C++ implementation.
#'
#' @inheritParams calc_exceedance_prob
#'
#' @return A numeric vector of exceedance probabilities. If `rm.zero = TRUE`,
#'   the returned vector has the same length as the input with `NA` at positions
#'   of zero or negative values.
#'
#' @seealso [calc_exceedance_prob()]
#'
#' @examples
#' rcpp_calc_exceedance_prob(c(10, 5, 0, 15, 8, NA, 0, 20))
#'
#' @export
rcpp_calc_exceedance_prob <- function(flow, rm.zero = FALSE) {
  if (!is.numeric(flow)) {
    stop("'flow' must be a numeric vector.")
  }

  if (any(is.infinite(flow))) {
    stop("'flow' contains infinite values. These are not allowed.")
  }

  if (!is.logical(rm.zero)) {
    stop("'rm.zero' must be a logical value (TRUE or FALSE).")
  }

  if (length(flow) == 0) {
    warning("'flow' is an empty vector. Returning an empty numeric vector.")
    return(numeric(0))
  }

  if (rm.zero && length(flow[flow > 0 & !is.na(flow)]) == 0) {
    warning(
      "All non-NA values in 'flow' were zero. Returning a vector of NAs with the original length."
    )
    return(rep(NA, length(flow)))
  }

  cpp_calc_exceedance_prob(flow, rm.zero)
}
