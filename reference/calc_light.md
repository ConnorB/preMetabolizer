# Calculate modeled photosynthetically active radiation

Calculate photosynthetically active radiation (PAR) for a series of
date-times and site coordinates. Input times should be mean solar time,
as expected by stream metabolism models.

## Usage

``` r
calc_light(solar.time, latitude, longitude, max.PAR = 2326)
```

## Arguments

- solar.time:

  POSIXct vector of mean solar time values.

- latitude:

  Numeric. Site latitude in decimal degrees between -90 and 90.

- longitude:

  Numeric longitude in decimal degrees. Western longitudes are negative.

- max.PAR:

  Numeric. Peak daily PAR in umol m^-2 s^-1. Defaults to `2326`.

## Value

Numeric vector of modeled PAR in umol m^-2 s^-1.

## Details

This function was adapted from
[`streamMetabolizer::calc_light`](https://rdrr.io/pkg/streamMetabolizer/man/calc_light.html)
and is included here under the terms of the CC0 1.0 Universal public
domain dedication, as described at:
https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits

Original citation: Appling, A. P., Hall, R. O., Yackulic, C. B., &
Arroita, M. (2018). Overcoming Equifinality: Leveraging Long Time Series
for Stream Metabolism Estimation. *Journal of Geophysical Research:
Biogeosciences*, 123(2), 624–645. https://doi.org/10.1002/2017JG004140

## Examples

``` r
utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
solar.time <- convert_UTC_to_solartime(utc, longitude = -96.6)

calc_light(solar.time, latitude = 39.1, longitude = -96.6)
#> [1] 2228.291
```
