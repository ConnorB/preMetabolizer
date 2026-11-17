# Calculate stream depth from USGS channel measurements

Downloads channel measurements for a USGS gage and fits the at-a-station
hydraulic geometry relationship \\D = cQ^f\\. Mean measured depth is
calculated as channel area divided by channel width.

## Usage

``` r
calc_usgs_depth(gage_number, discharge)
```

## Arguments

- gage_number:

  Character scalar. USGS gage number, with or without the `"USGS-"`
  prefix.

- discharge:

  Numeric vector. Discharge in cubic feet per second (cfs). Values must
  be positive; missing values are returned as missing depths.

## Value

A numeric vector of estimated mean stream depths in meters.

## Details

Channel measurements are retrieved with
[`dataRetrieval::read_waterdata_channel()`](https://rdrr.io/pkg/dataRetrieval/man/read_waterdata_channel.html).
Measurements without positive discharge, width, or area are omitted.
Multiple channels recorded under one measurement are combined before the
power-law relationship is fitted.

## References

Leopold, L. B., and Maddock, T. Jr. (1953). *The Hydraulic Geometry of
Stream Channels and Some Physiographic Implications*. USGS Professional
Paper 252. [doi:10.3133/pp252](https://doi.org/10.3133/pp252)

## Examples

``` r
if (FALSE) { # \dontrun{
calc_usgs_depth("06879650", c(0.1, 1, 10))
} # }
```
