% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flag_outliers.R
\name{flag_outlier}
\alias{flag_outlier}
\title{Rolling Z-Score Calculation}
\usage{
flag_outlier(x, window_size = 30)
}
\arguments{
\item{x}{A numeric vector.}

\item{window_size}{An integer specifying the size of the rolling window (default is 30).}
}
\value{
A vector of integers (0 or 1) indicating whether each value in \code{x} is an outlier (1) or not (0).
}
\description{
Computes rolling z-scores for a numeric vector, identifying outliers based on a specified window size.
}
\keyword{internal}
