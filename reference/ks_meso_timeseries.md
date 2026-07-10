# Get Kansas Mesonet time-series data (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`ks_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_time_series.md)
instead.

## Usage

``` r
ks_meso_timeseries(
  stations = NULL,
  network = NULL,
  start_date,
  end_date,
  interval,
  vars = NULL
)
```

## Examples

``` r
# Old:
# ks_meso_timeseries(stations = "Konza Prairie", start_date = "2024-06-01",
#   end_date = "2024-06-07", interval = "hour")
# New:
if (FALSE) { # \dontrun{
ks_meso_time_series(
  stations = "Konza Prairie",
  start_date = "2024-06-01",
  end_date = "2024-06-07",
  interval = "hour"
)
} # }
```
