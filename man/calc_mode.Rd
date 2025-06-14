% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_mode.R
\name{calc_mode}
\alias{calc_mode}
\title{Calculate the Mode(s) of a Vector}
\usage{
calc_mode(x, na.rm = TRUE, multi = "first")
}
\arguments{
\item{x}{An atomic vector (numeric, character, factor, etc.) for which to compute the mode.}

\item{na.rm}{Logical indicating whether to remove NA values before computation (default: \code{TRUE}).}

\item{multi}{Method to handle multiple modes when they exist. Options are:
\describe{
\item{"first"}{Returns the first occurring mode (default)}
\item{"last"}{Returns the last occurring mode}
\item{"sample"}{Returns one random mode}
\item{"all"}{Returns all modes as a vector}
}}
}
\value{
A value or vector of the same type as \code{x} containing:
\itemize{
\item A single mode (default behavior)
\item All modes if \code{multi = "all"} and multiple modes exist
\item \code{NA} if:
\itemize{
\item Input is empty
\item Input contains only NAs (when \code{na.rm = TRUE})
\item No mode can be determined
}
}
}
\description{
Computes the mode (most frequent value) of a vector. By default returns a single mode
even when multiple modes exist. Handles NA values and preserves the input data type.
}
\details{
For factors, the returned mode(s) maintain the original factor levels. When \code{na.rm = FALSE}
and NAs are present, the function returns \code{NA}. The function handles ties (multiple values
with the same maximum frequency) according to the \code{multi} parameter.
}
\examples{
# Single mode
calc_mode(c(1, 2, 2, 3, 3, 3))  # returns 3

# Multiple modes (returns first by default)
calc_mode(c(1, 1, 2, 2, 3))     # returns 1

# Multiple modes with different handling
calc_mode(c(1, 1, 2, 2, 3), multi = "last")   # returns 2
calc_mode(c(1, 1, 2, 2, 3), multi = "sample") # returns 1 or 2 randomly
calc_mode(c(1, 1, 2, 2, 3), multi = "all")    # returns c(1, 2)

# Factor vector
fruit <- factor(c("apple", "banana", "banana", "cherry"))
calc_mode(fruit)  # returns "banana" (factor level maintained)

# With NA values
calc_mode(c(1, 2, 2, NA))        # returns 2
calc_mode(c(NA, NA), na.rm = FALSE) # returns NA

# Edge cases
calc_mode(integer(0))            # returns NA
calc_mode(c(NA, NA))             # returns NA
calc_mode(c(1))                  # returns 1

}
\seealso{
\code{\link[=table]{table()}} for frequency tables, \code{\link[=which.max]{which.max()}} for single maximum values
}
