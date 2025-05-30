% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_O2sat.R
\name{calc_O2sat}
\alias{calc_O2sat}
\title{Calculate Dissolved Oxygen Saturation}
\usage{
calc_O2sat(temp_water, atmo_press, units = "atm", salinity = 0)
}
\arguments{
\item{temp_water}{Numeric. Water temperature in degrees Celsius.}

\item{atmo_press}{Numeric. Barometric pressure values.}

\item{units}{Character. Units of barometric pressure. Accepted values are
\code{"atm"}, \code{"hPa"}, \code{"mbar"}, and \code{"kPa"}.
Default is \code{"atm"}.}

\item{salinity}{Numeric. salinity in parts per thousand (ppt). Default is \code{0}.}
}
\value{
Numeric. Dissolved oxygen saturation ([DO]) in mg/L.
}
\description{
This function calculates dissolved oxygen saturation ([DO]) in water,
accounting for temperature, barometric pressure, and salinity.
It uses the Garcia and Gordon (1992) equations and corrects for vapor
pressure and water density.
}
\details{
The function calculates the dissolved oxygen saturation based on the Garcia
and Gordon (1992) model. It includes corrections for:
\itemize{
\item Barometric pressure, converted from the specified units to atm.
\item Vapor pressure of water, using the Antoine equation.
\item Water density, dynamically calculated based on temperature.
}
}
\examples{
# Example with mbar
temp <- 25  # Water temperature in °C
atmo_mbar <- c(1013, 1015, 1012)  # Barometric pressure in mbar
calc_O2sat(temp_water = temp, atmo_press = atmo_mbar, units = "mbar")

# Example with kPa
temp <- 20
atmo_kPa <- c(101.3, 101.5, 101.2)  # Barometric pressure in kPa
calc_O2sat(temp_water = temp, atmo_press = atmo_kPa, units = "kPa")

}
