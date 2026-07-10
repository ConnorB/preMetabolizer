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
#> # A tibble: 4 × 27
#>   air_temp air_temp9_m humidity precip wind_speed wind_speed2_m wind_speed10_m
#>      <dbl>       <dbl>    <dbl>  <dbl>      <dbl>         <dbl>          <dbl>
#> 1     32.2          NA     43.9      0       3.43          3.43             NA
#> 2     31.3          NA     45.6      0       2.19          2.19             NA
#> 3     32.7          NA     42.7      0       1.89          1.89             NA
#> 4     32.4          NA     42.0      0       3.29          3.29             NA
#> # ℹ 20 more variables: wind_direction <dbl>, wind_direction2_m <dbl>,
#> #   wind_direction10_m <dbl>, wind_gust2_m <dbl>, wind_gust10_m <dbl>,
#> #   air_pressure <dbl>, solar_radiation <dbl>, soil_moist5 <dbl>,
#> #   soil_moist10 <dbl>, soil_moist20 <dbl>, soil_moist50 <dbl>,
#> #   soil_temp5 <dbl>, soil_temp10 <dbl>, soil_temp20 <dbl>, soil_temp50 <dbl>,
#> #   water_level <dbl>, water_temp <dbl>, water_level2 <dbl>, water_temp2 <dbl>,
#> #   date_time <dttm>
```
