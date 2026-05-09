# Calculate modeled light from solar.time

Calculate photosynthetically active radiation (PAR) for a series of
date-times and site coordinates. This function was adapted from
[`streamMetabolizer::calc_light`](https://rdrr.io/pkg/streamMetabolizer/man/calc_light.html)
and is included here under the terms of the CC0 1.0 Universal public
domain dedication, as described at:
https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits

## Usage

``` r
calc_light(solar.time, latitude, longitude, max.PAR = 2326)
```

## Arguments

- solar.time:

  mean solar time, as required for input to metabolism models.

- latitude:

  numeric. Site latitude in decimal degrees between -90 and 90.

- longitude:

  Numeric longitude in decimal degrees (east positive, west negative).

- max.PAR:

  numeric: the PAR (umol m^-2 s^-1) that each day should reach at peak
  light

## Details

Original citation: Appling, A. P., Hall, R. O., Yackulic, C. B., &
Arroita, M. (2018). Overcoming Equifinality: Leveraging Long Time Series
for Stream Metabolism Estimation. *Journal of Geophysical Research:
Biogeosciences*, 123(2), 624–645. https://doi.org/10.1002/2017JG004140

## Examples

``` r
solar.time <- lubridate::force_tz(as.POSIXct('2016-09-27 12:00'), 'UTC')
calc_light(solar.time, 40, -120)
#> [1] 1721.038
```
