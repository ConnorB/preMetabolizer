#' Calculate the mode of a vector
#'
#' Finds the most frequent value in an atomic vector. Ties can be handled by
#' returning the first mode, last mode, a random mode, or all modes.
#'
#' @param x Atomic vector, such as numeric, character, logical, or factor.
#' @param na.rm Logical. If `TRUE`, remove missing values before calculation.
#'   Defaults to `TRUE`.
#' @param multi Character string controlling ties. Options are:
#'   \describe{
#'     \item{`"first"`}{Return the first mode in sorted table order.}
#'     \item{`"last"`}{Return the last mode in sorted table order.}
#'     \item{`"sample"`}{Return one randomly selected mode.}
#'     \item{`"all"`}{Return all tied modes.}
#'   }
#'
#' @return A value or vector with the same general type as `x`. Returns `NA`
#'   for empty inputs or inputs containing only missing values after `NA`
#'   removal.
#'
#' @details
#' For factors, returned mode values preserve the original levels. When
#' `na.rm = FALSE`, missing values are included in the frequency table.
#'
#' @examples
#' calc_mode(c(1, 2, 2, 3, 3, 3))
#'
#' tied <- c("riffle", "run", "riffle", "pool", "run")
#' calc_mode(tied)
#' calc_mode(tied, multi = "all")
#'
#' fruit <- factor(c("apple", "banana", "banana", "cherry"))
#' calc_mode(fruit)
#'
#' @export
#' @seealso [table()] for frequency tables.
calc_mode <- function(x, na.rm = TRUE, multi = "first") {
  if (!is.atomic(x)) {
    cli::cli_abort("{.arg x} must be an atomic vector.")
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(NA)
  }

  freq_table <- table(x)
  max_freq <- max(freq_table)
  modes <- names(freq_table[freq_table == max_freq])

  # Handle multiple modes
  if (length(modes) > 1) {
    modes <- switch(
      multi,
      "first" = modes[1],
      "last" = modes[length(modes)],
      "sample" = sample(modes, 1),
      modes
    ) # default returns all
  }

  # Convert back to original type
  if (is.factor(x)) {
    modes <- factor(modes, levels = levels(x))
  } else {
    modes <- suppressWarnings(methods::as(modes, class(x)))
  }

  modes
}
