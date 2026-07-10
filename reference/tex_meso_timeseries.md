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
tex_meso_time_series(2, prior_minutes = 60)
#> # A tibble: 5 × 27
#>   air_temp air_temp9_m humidity precip wind_speed wind_speed2_m wind_speed10_m
#>      <dbl>       <dbl>    <dbl>  <dbl>      <dbl>         <dbl>          <dbl>
#> 1     27.1          NA     56.8      0       2.34          2.34             NA
#> 2     27.4          NA     56.2      0       2.62          2.62             NA
#> 3     27.7          NA     55.1      0       2.98          2.98             NA
#> 4     28.0          NA     53.5      0       2.07          2.07             NA
#> 5     28.2          NA     52.9      0       1.84          1.84             NA
#> # ℹ 20 more variables: wind_direction <dbl>, wind_direction2_m <dbl>,
#> #   wind_direction10_m <dbl>, wind_gust2_m <dbl>, wind_gust10_m <dbl>,
#> #   air_pressure <dbl>, solar_radiation <dbl>, soil_moist5 <dbl>,
#> #   soil_moist10 <dbl>, soil_moist20 <dbl>, soil_moist50 <dbl>,
#> #   soil_temp5 <dbl>, soil_temp10 <dbl>, soil_temp20 <dbl>, soil_temp50 <dbl>,
#> #   water_level <dbl>, water_temp <dbl>, water_level2 <dbl>, water_temp2 <dbl>,
#> #   date_time <dttm>
```
