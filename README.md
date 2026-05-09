
# preMetabolizer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL (>=
3)](https://img.shields.io/badge/license-GPL%20(%3E%3D%203)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/ConnorB/preMetabolizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ConnorB/preMetabolizer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`preMetabolizer` is a utility R package that provides tools for
wrangling the input data required by
[`streamMetabolizer`](https://github.com/ConnorB/streamMetabolizer).

## Installation

`preMetabolizer` is not on CRAN. Install the development version from
GitHub:

``` r
# install.packages("pak")
pak::pak("ConnorB/preMetabolizer")
```

Or with `remotes`:

``` r
# install.packages("remotes")
remotes::install_github("ConnorB/preMetabolizer")
```

The package requires R (\>= 4.1.0). A companion fork of
`streamMetabolizer` is listed under `Remotes:` and will be installed
automatically.

## Usage

### Physical and chemical calculations

``` r
library(preMetabolizer)

# Dissolved oxygen saturation at 15°C, standard atmosphere, freshwater
calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
#> [1] 10.07807

# Water density (kg/m³) at several temperatures
calc_water_density(water_temp = c(0, 10, 20, 30))
#> [1] 999.8395 999.6996 998.2041 995.6473

# Barometric pressure correction from a station at 300 m to a site at 500 m
correct_bp(station_bp = 101.3, air_temp = 15, station_elev = 300, site_elev = 500)
#> [1] 98.92626
```

### Unit conversion

``` r
# Stream discharge: cfs → cms
convert_flow(14.32, from = "cfs", to = "cms")
#> [1] 0.4054972

# Barometric pressure: kPa → atm
convert_pressure(101.325, from = "kPa", to = "atm")
#> [1] 1
```

### Solar radiation and light

``` r
# Convert a UTC timestamp to mean solar time at a site in Kansas
utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
solar <- convert_UTC_to_solartime(utc, longitude = -96.6, time.type = "mean solar")

# Model PAR at solar noon on the summer solstice
noon_solar <- convert_UTC_to_solartime(
  as.POSIXct("2024-06-21 12:00:00", tz = "UTC"),
  longitude = -96.6
)
calc_light(noon_solar, latitude = 39.1, longitude = -96.6)
#> [1] 390.264
```

### Time series utilities

``` r
# Flag outliers with a moving-window robust Z-score
x <- c(1.0, 1.2, 1.1, 1.3, 50.0, 1.1, 1.2, 1.0)
flag_z(x, width = 5, threshold = 3)
#> [1] NA  NA  NA  NA  "Z" NA  NA  NA

# Determine meteorological season from a date
get_season(c("2024-01-15", "2024-04-15", "2024-07-04", "2024-10-15"))
#> [1] "Winter" "Spring" "Summer" "Fall"
```

## License

GPL (\>= 3). See [LICENSE.md](LICENSE.md).

## Issues

Bug reports and feature requests are welcome at
<https://github.com/ConnorB/preMetabolizer/issues>.
