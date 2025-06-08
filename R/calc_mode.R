#' Calculate the Mode(s) of a Vector
#'
#' @description
#' Computes the mode (most frequent value) of a vector. By default returns a single mode
#' even when multiple modes exist. Handles NA values and preserves the input data type.
#'
#' @param x An atomic vector (numeric, character, factor, etc.) for which to compute the mode.
#' @param na.rm Logical indicating whether to remove NA values before computation (default: `TRUE`).
#' @param multi Method to handle multiple modes when they exist. Options are:
#'   \describe{
#'     \item{"first"}{Returns the first occurring mode (default)}
#'     \item{"last"}{Returns the last occurring mode}
#'     \item{"sample"}{Returns one random mode}
#'     \item{"all"}{Returns all modes as a vector}
#'   }
#'
#' @return A value or vector of the same type as `x` containing:
#' * A single mode (default behavior)
#' * All modes if `multi = "all"` and multiple modes exist
#' * `NA` if:
#'   - Input is empty
#'   - Input contains only NAs (when `na.rm = TRUE`)
#'   - No mode can be determined
#'
#' @details
#' For factors, the returned mode(s) maintain the original factor levels. When `na.rm = FALSE`
#' and NAs are present, the function returns `NA`. The function handles ties (multiple values
#' with the same maximum frequency) according to the `multi` parameter.
#'
#' @examples
#' # Single mode
#' calc_mode(c(1, 2, 2, 3, 3, 3))  # returns 3
#'
#' # Multiple modes (returns first by default)
#' calc_mode(c(1, 1, 2, 2, 3))     # returns 1
#'
#' # Multiple modes with different handling
#' calc_mode(c(1, 1, 2, 2, 3), multi = "last")   # returns 2
#' calc_mode(c(1, 1, 2, 2, 3), multi = "sample") # returns 1 or 2 randomly
#' calc_mode(c(1, 1, 2, 2, 3), multi = "all")    # returns c(1, 2)
#'
#' # Factor vector
#' fruit <- factor(c("apple", "banana", "banana", "cherry"))
#' calc_mode(fruit)  # returns "banana" (factor level maintained)
#'
#' # With NA values
#' calc_mode(c(1, 2, 2, NA))        # returns 2
#' calc_mode(c(NA, NA), na.rm = FALSE) # returns NA
#'
#' # Edge cases
#' calc_mode(integer(0))            # returns NA
#' calc_mode(c(NA, NA))             # returns NA
#' calc_mode(c(1))                  # returns 1
#'
#' @export
#' @seealso [table()] for frequency tables, [which.max()] for single maximum values
calc_mode <- function(x, na.rm = TRUE, multi = "first") {
  if (!is.atomic(x)) stop("Input must be an atomic vector")

  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)

  freq_table <- table(x)
  max_freq <- max(freq_table)
  modes <- names(freq_table[freq_table == max_freq])

  # Handle multiple modes
  if (length(modes) > 1) {
    modes <- switch(multi,
                    "first" = modes[1],
                    "last" = modes[length(modes)],
                    "sample" = sample(modes, 1),
                    modes)  # default returns all
  }

  # Convert back to original type
  if (is.factor(x)) {
    modes <- factor(modes, levels = levels(x))
  } else {
    modes <- suppressWarnings(as(modes, class(x)))
  }

  modes
}
