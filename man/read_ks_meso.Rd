% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ks_mesonet.R
\name{read_ks_meso}
\alias{read_ks_meso}
\title{Read Cached Kansas Mesonet Data}
\usage{
read_ks_meso(station, start_date, end_date, interval, output_dir = NULL)
}
\arguments{
\item{station}{Station name as a character string.}

\item{start_date}{Start date for the data in \code{YYYY-MM-DD} format.}

\item{end_date}{End date for the data in \code{YYYY-MM-DD} format.}

\item{interval}{Data interval. Must be one of \code{'hour'}, \code{'5min'}, or \code{'day'}.}

\item{output_dir}{Directory where the cached data is stored. Defaults to the cache path.}
}
\value{
A data frame containing the requested Mesonet data.
}
\description{
Reads previously downloaded and cached Kansas Mesonet data.
}
