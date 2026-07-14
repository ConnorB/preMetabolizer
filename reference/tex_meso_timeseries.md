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
#> 1     25.6          NA     73.5      0      0.522         0.522             NA
#> 2     25.6          NA     71.8      0      0.940         0.940             NA
#> 3     25.5          NA     70.3      0      1.87          1.87              NA
#> 4     25.4          NA     70.0      0      0             0                 NA
#> 5     25.3          NA     72.0      0      0.462         0.462             NA
#> # ℹ 20 more variables: wind_direction <dbl>, wind_direction2_m <dbl>,
#> #   wind_direction10_m <dbl>, wind_gust2_m <dbl>, wind_gust10_m <dbl>,
#> #   air_pressure <dbl>, solar_radiation <dbl>, soil_moist5 <dbl>,
#> #   soil_moist10 <dbl>, soil_moist20 <dbl>, soil_moist50 <dbl>,
#> #   soil_temp5 <dbl>, soil_temp10 <dbl>, soil_temp20 <dbl>, soil_temp50 <dbl>,
#> #   water_level <dbl>, water_temp <dbl>, water_level2 <dbl>, water_temp2 <dbl>,
#> #   date_time <dttm>
```
