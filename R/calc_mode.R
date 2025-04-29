#' Calculate the Mode(s) of a Vector
#'
#' @description
#' Computes the mode (most frequent value) of a vector. Handles multiple modes, NA values,
#' and preserves the input data type. For empty inputs or all-NA inputs, returns NA.
#'
#' @param x A vector (numeric, character, factor, etc.) for which to compute the mode(s).
#' @param na.rm Logical indicating whether to remove NA values before computation (default: TRUE).
#'
#' @return A vector of the same type as `x` containing the mode(s). Returns NA if:
#' - Input is empty
#' - Input contains only NAs (when na.rm = TRUE)
#' - No mode can be determined
#'
#' @examples
#' # Single mode
#' calc_mode(c(1, 2, 2, 3, 3, 3))  # 3
#'
#' # Multiple modes
#' calc_mode(c(1, 1, 2, 2, 3))     # c(1, 2)
#'
#' # Character vector
#' calc_mode(c("a", "b", "b", "c")) # "b"
#'
#' # With NA values
#' calc_mode(c(1, 2, 2, NA))        # 2
#' calc_mode(c(NA, NA), na.rm = FALSE) # NA
#'
#' # Edge cases
#' calc_mode(integer(0))            # NA
#' calc_mode(c(NA, NA))             # NA
#'
#' @export
calc_mode <- function(x, na.rm = TRUE) {
  if (!is.atomic(x)) {
    stop("Input must be an atomic vector")
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }

  if (length(x) == 0) {
    return(NA)
  }

  freq_table <- table(x)
  max_freq <- max(freq_table)
  modes <- names(freq_table[freq_table == max_freq])

  # Convert back to original type while preserving attributes
  if (is.factor(x)) {
    modes <- factor(modes, levels = levels(x))
  } else {
    modes <- suppressWarnings(as(modes, class(x)))
  }

  modes
}

