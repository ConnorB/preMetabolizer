# Get recent TexMesonet time-series data (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`tex_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_time_series.md)
instead.

## Usage

``` r
tex_meso_timeseries(
  site_id,
  prior_minutes,
  variable = c("all", "temperature", "humidity", "barometric_pressure", "precip",
    "wind_speed")
)
```

## Examples

``` r
# Old:
# tex_meso_timeseries(2, prior_minutes = 60)
# New:
if (FALSE) { # \dontrun{
tex_meso_time_series(2, prior_minutes = 60)
} # }
```
