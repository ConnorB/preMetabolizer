#' Calculate Flow Exceedence Probabilities
#'
#' This function calculates the exceedance probability for a given numeric vector of flow values.
#' The exceedance probability is calculated using the Weibull plotting position formula.
#'
#' @param flow A numeric vector of flow (discharge) values.
#' @param rm.Zero Logical. If `TRUE`, zero values are removed from the calculation, and
#'                positions corresponding to zeros are filled with `NA` in the output.
#'
#' @return A numeric vector of exceedance probabilities. If `rm.Zero = TRUE`, the returned vector
#'         will have the same length as the input with `NA` at positions of zero values.
#'
#' @details The Weibull plotting position is used to compute the exceedance probability:
#'          \deqn{P = \frac{\text{rank}}{n + 1}}
#'          where `rank` is the rank of the flow value (in descending order), and `n` is the
#'          number of observations.
#'
#' @examples
#' flow_data <- c(10, 5, 0, 15, 8, 0, 20)
#' exceedance_probs <- calc_exceedance_prob(flow_data)
#' exceedance_probs_no_zeros <- calc_exceedance_prob(flow_data, rm.Zero = TRUE)
#'
#' @export
calc_exceedance_prob <- function(flow, rm.Zero = FALSE) {

  # Input type and validity checks
  if (!is.numeric(flow)) {
    stop("The input 'flow' must be a numeric vector.")
  }

  if (any(is.na(flow))) {
    warning("Input 'flow' contains NA values. These will be included in the ranking, but the resulting exceedance probabilities may not be meaningful. Consider removing NAs before using this function.")
  }

  if (any(is.infinite(flow))) {
    stop("Input 'flow' contains infinite values. These are not allowed.")
  }

  if (!is.logical(rm.Zero)) {
    stop("The input 'rm.Zero' must be a logical value (TRUE or FALSE).")
  }

  if (length(flow) == 0){
    warning("Input 'flow' is an empty vector. Returning an empty numeric vector.")
    return(numeric(0))
  }

  # Store the original length and indices
  original_length <- length(flow)
  non_zero_indices <- which(flow > 0)

  if (rm.Zero) {
    # Remove zeros
    flow <- flow[flow > 0]

    if (length(flow) == 0){
      warning("All values in 'flow' were zero. Returning a vector of NAs with the original length")
      return(rep(NA, original_length))
    }
  }

  # Calculate the rank of each flow value (highest to lowest)
  ranked_flow <- rank(-flow, ties.method = "random")

  # Calculate the number of observations
  n <- length(flow)

  # Calculate the exceedance probability using the Weibull plotting position
  exceedance_probability <- ranked_flow / (n + 1)

  # If rm.Zero is TRUE, restore the full length with NA for zero-value positions
  if (rm.Zero) {
    full_exceedance_prob <- rep(NA, original_length)
    full_exceedance_prob[non_zero_indices] <- exceedance_probability
    return(full_exceedance_prob)
  }

  return(exceedance_probability)
}
