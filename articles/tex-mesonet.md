# Working with TexMesonet data

## Introduction

[TexMesonet](https://www.texmesonet.org/) is a statewide earth
observation network managed by the Texas Water Development Board (TWDB).
TWDB stations collect near-real-time weather and soil observations,
including air temperature, humidity, precipitation, wind, solar
radiation, soil temperature, and soil moisture.

preMetabolizer provides three helpers for the public TexMesonet API:

- [`tex_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_stations.md)
  retrieves TWDB station metadata.
- [`tex_meso_current()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_current.md)
  retrieves the most recent observation from each TWDB station.
- [`tex_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_time_series.md)
  retrieves recent time-series observations for one station.

> **Note:** These functions contact the TexMesonet API and require an
> internet connection. Code chunks that call the API will not run during
> package installation if the service is unreachable. Run them
> interactively in your own session.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Caching downloaded data

Repeated calls to the TexMesonet API download the same data on every
run. This vignette saves each result to a local cache directory the
first time it is downloaded and reloads from disk on subsequent runs.
The cache lives in
`tools::R_user_dir("preMetabolizer", which = "cache")`, a
platform-appropriate, user-specific directory that persists across
sessions. Each data-fetching chunk below checks for a cached `.rds`
file, downloads and saves on the first run, and reloads from disk on all
subsequent runs.

## Discover TWDB stations

Use
[`tex_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_stations.md)
to retrieve station names, IDs, display IDs, coordinates, elevation,
activity status, and online dates.

``` r

cache_file <- file.path(cache_dir, "tex_meso_stations.rds")
if (!file.exists(cache_file)) {
  stations <- tex_meso_stations()
  saveRDS(stations, cache_file)
} else {
  stations <- readRDS(cache_file)
}

glimpse(stations)
#> Rows: 138
#> Columns: 12
#> $ station_id      <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ station_name    <chr> "Blanco Weather Station", "Altwein Rd", "Headwaters Ra…
#> $ display_id      <chr> "BCBWS", "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", …
#> $ state           <chr> "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", "TX", …
#> $ county          <chr> "Blanco", "Blanco", "Kendall", "Blanco", "Blanco", "Ba…
#> $ latitude        <dbl> 30.08906, 30.14957, 30.08845, 30.01853, 30.11047, 29.8…
#> $ longitude       <dbl> -98.41806, -98.54044, -98.69757, -98.45631, -98.30463,…
#> $ elevation       <int> 1334, 1730, 1959, 1416, 1316, 2163, 2206, 1881, 166, 3…
#> $ active          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,…
#> $ station_type    <int> 1, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, …
#> $ station_display <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,…
#> $ online_date     <date> 2016-04-28, 2016-04-28, 2016-04-28, 2016-04-28, 2016-…
```

The `active` and `displayed` arguments make it easy to focus on stations
that are currently operating and shown by TexMesonet.

``` r

cache_file <- file.path(cache_dir, "tex_meso_stations_active.rds")
if (!file.exists(cache_file)) {
  active_stations <- tex_meso_stations(active = TRUE, displayed = TRUE)
  saveRDS(active_stations, cache_file)
} else {
  active_stations <- readRDS(cache_file)
}

active_stations |>
  select(station_id, station_name, display_id, county, latitude, longitude) |>
  arrange(county, station_name)
#> # A tibble: 126 × 6
#>    station_id station_name            display_id county  latitude longitude
#>         <int> <chr>                   <chr>      <chr>      <dbl>     <dbl>
#>  1          6 Love Creek Preserve     BNLCP      Bandera     29.8     -99.4
#>  2         69 Phyllis Thomas Bandera  BNPTB      Bandera     29.7     -99.4
#>  3         37 Pecan Grove Farms       BPPGF      Bastrop     30.2     -97.5
#>  4        102 Pawnee Ranch            BEPAW      Bee         28.6     -98.0
#>  5         34 City of Rogers          BLCOR      Bell        30.9     -97.2
#>  6         33 City of Troy            BLCOT      Bell        31.2     -97.3
#>  7         35 Doc Curb Pump Station   BLDOC      Bell        31.0     -97.5
#>  8         36 River Ridge Ranch       BLRRR      Bell        31.0     -97.8
#>  9        107 EAA Field Research Park BXEAA      Bexar       29.7     -98.4
#> 10          2 Altwein Rd              BCALT      Blanco      30.1     -98.5
#> # ℹ 116 more rows
```

For station time-series requests, keep the integer `station_id`. This
example finds stations in Blanco County and selects one station ID for
later use.

``` r

blanco_stations <- active_stations |>
  filter(county == "Blanco") |>
  select(station_id, station_name, display_id, latitude, longitude, elevation)

blanco_stations
#> # A tibble: 4 × 6
#>   station_id station_name         display_id latitude longitude elevation
#>        <int> <chr>                <chr>         <dbl>     <dbl>     <int>
#> 1          2 Altwein Rd           BCALT          30.1     -98.5      1730
#> 2          4 Holt Oaks Ranch      BCHOR          30.0     -98.5      1416
#> 3          5 Arnosky Farms        BCARN          30.1     -98.3      1316
#> 4        108 Blanco Middle School BCMID          30.1     -98.4      1395

site_id <- blanco_stations$station_id[1]
```

## Retrieve current observations

[`tex_meso_current()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_current.md)
returns the most recent available observation from each TWDB station.
Not every station measures every parameter, so some columns may contain
missing values.

``` r

cache_file <- file.path(cache_dir, "tex_meso_current.rds")
if (!file.exists(cache_file)) {
  current <- tex_meso_current()
  saveRDS(current, cache_file)
} else {
  current <- readRDS(cache_file)
}

glimpse(current)
#> Rows: 125
#> Columns: 39
#> $ object_id             <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_id            <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "RE…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 2206, 1881, 166, 3…
#> $ air_temp              <dbl> 24.80, 22.94, 24.91, 25.59, 23.83, 22.97, 32.58,…
#> $ air_temp2_m           <dbl> 24.80, 22.94, 24.91, 25.59, 23.83, 22.97, 32.58,…
#> $ humidity              <dbl> 87.70, 96.60, 90.20, 88.70, 87.90, 100.00, 45.60…
#> $ precip                <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.254, 0.000,…
#> $ precip24_hr           <dbl> 1.016, 0.000, 10.414, 5.080, 23.368, 62.992, 4.3…
#> $ precip48_hr           <dbl> 1.016, 0.000, 10.414, 5.080, 23.368, 62.992, 4.3…
#> $ precip72_hr           <dbl> 1.016, 0.000, 10.414, 5.080, 23.368, 62.992, 4.3…
#> $ wind_speed            <dbl> 0.3442, 0.4292, 0.0000, 1.4367, 2.9919, 0.5400, …
#> $ wind_speed2_m         <dbl> 0.3442, 0.4292, 0.0000, 1.4367, 1.9700, 0.5400, …
#> $ wind_direction        <dbl> 160, 142, 149, 190, 129, 145, 165, 276, 104, 127…
#> $ wind_direction2_m     <dbl> 160, 142, 149, 190, 51, 145, 165, 298, 104, 127,…
#> $ wind_gust             <dbl> 3.7419, 1.8778, 0.0000, 2.9430, 3.8318, 1.6115, …
#> $ wind_gust2_m          <dbl> 3.7419, 1.8778, 0.0000, 2.9430, 3.2093, 1.6115, …
#> $ battery_voltage       <dbl> 13.06, 13.03, 13.43, 13.61, 13.34, 13.14, 13.35,…
#> $ soil_moisture         <dbl> 0.212, 0.301, 0.199, 0.428, 0.313, 0.565, NA, 0.…
#> $ soil_temperature      <dbl> 30.37, 25.89, 28.76, 27.64, 28.71, 27.12, NA, 26…
#> $ soil_moisture5_cm     <dbl> 0.128, 0.186, 0.293, 0.194, 0.225, 0.144, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 30.02, 26.77, 30.39, 38.21, 29.18, 26.01, NA, 26…
#> $ soil_moisture10_cm    <dbl> 0.159, 0.122, 0.211, 0.368, 0.392, 0.313, NA, 0.…
#> $ soil_temperature10_cm <dbl> 30.13, 26.43, 29.87, 28.35, 28.56, 26.49, NA, 26…
#> $ soil_moisture20_cm    <dbl> 0.212, 0.301, 0.199, 0.428, 0.313, 0.565, NA, 0.…
#> $ soil_temperature20_cm <dbl> 30.37, 25.89, 28.76, 27.64, 28.71, 27.12, NA, 26…
#> $ soil_moisture50_cm    <dbl> 0.236, NA, 0.233, 0.520, NA, NA, NA, 0.173, NA, …
#> $ soil_temperature50_cm <dbl> 29.43, NA, 27.79, NA, NA, NA, NA, 26.90, NA, 28.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-07-11 20:10:00, 2026-07-11 20:10:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 23.58, NA, 31.86, 27.60, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 2.9919, NA, NA, 0.0009, NA, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 129, NA, NA, 276, NA, NA, 98, 81…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 3.8318, NA, NA, 0.0555, NA, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 943.2614, NA, 949.0303, 1011.722…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 111.4483, NA, 1135.9870, 794.378…
```

The TexMesonet API reports units in a separate object. preMetabolizer
stores that object as a data-frame attribute.

``` r

attr(current, "units")
#> $air_pressure
#> [1] "Millibars"
#> 
#> $air_temp
#> [1] "Celsius"
#> 
#> $battery_voltage
#> [1] "Volts"
#> 
#> $data_interval
#> [1] "Minutes"
#> 
#> $elevation
#> [1] "Feet"
#> 
#> $precipitation
#> [1] "Millimeters"
#> 
#> $relative_humidity
#> [1] "Percent"
#> 
#> $soil_moisture
#> [1] "Centimeters Cubed per Centimeters Cubed"
#> 
#> $soil_temp
#> [1] "Celsius"
#> 
#> $solar_radiation
#> [1] "Watts per Square Meter"
#> 
#> $wind_speed
#> [1] "Meters per Second"
```

For a quick station check, join current observations to station metadata
or filter directly by `station_id`.

``` r

current |>
  filter(station_id == site_id) |>
  select(
    station_id,
    station_name,
    recorded_time,
    air_temp,
    humidity,
    precip,
    wind_speed,
    air_pressure
  )
#> # A tibble: 1 × 8
#>   station_id station_name recorded_time       air_temp humidity precip
#>        <int> <chr>        <dttm>                 <dbl>    <dbl>  <dbl>
#> 1          2 Altwein Rd   2026-07-11 20:10:00     24.8     87.7      0
#> # ℹ 2 more variables: wind_speed <dbl>, air_pressure <dbl>
```

## Retrieve recent time series

[`tex_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_time_series.md)
retrieves observations for a single station over a look-back window
measured in minutes. By default, `variable = "all"` uses the TexMesonet
charting-fields endpoint and returns all fields available for that
station.

``` r

cache_file <- file.path(cache_dir, "tex_meso_time_series_blanco.rds")
if (!file.exists(cache_file)) {
  recent <- tex_meso_time_series(
    site_id = site_id,
    prior_minutes = 24 * 60
  )
  saveRDS(recent, cache_file)
} else {
  recent <- readRDS(cache_file)
}

glimpse(recent)
#> Rows: 96
#> Columns: 27
#> $ air_temp           <dbl> 24.98, 25.12, 25.30, 25.69, 25.78, 25.38, 24.45, 24…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 86.20, 85.30, 84.60, 83.40, 83.50, 84.40, 86.70, 85…
#> $ precip             <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.…
#> $ wind_speed         <dbl> 0.0000, 0.0000, 2.1952, 1.7256, 0.5828, 0.9364, 0.4…
#> $ wind_speed2_m      <dbl> 0.0000, 0.0000, 2.1952, 1.7256, 0.5828, 0.9364, 0.4…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 140, 74, 166, 196, 65, 174, 192, 161, 158, 169, 133…
#> $ wind_direction2_m  <dbl> 140, 74, 166, 196, 65, 174, 192, 161, 158, 169, 133…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 0.0000, 0.0000, 4.2745, 4.8071, 2.9430, 4.2745, 2.9…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.128, 0.128, 0.128, 0.128, 0.128, 0.128, 0.128, 0.…
#> $ soil_moist10       <dbl> 0.160, 0.159, 0.160, 0.160, 0.160, 0.160, 0.160, 0.…
#> $ soil_moist20       <dbl> 0.212, 0.212, 0.212, 0.212, 0.212, 0.212, 0.212, 0.…
#> $ soil_moist50       <dbl> 0.236, 0.236, 0.236, 0.236, 0.236, 0.236, 0.236, 0.…
#> $ soil_temp5         <dbl> 30.00, 30.03, 29.97, 29.91, 29.89, 29.91, 29.90, 29…
#> $ soil_temp10        <dbl> 30.11, 30.12, 30.09, 30.05, 30.07, 30.06, 30.06, 30…
#> $ soil_temp20        <dbl> 30.37, 30.37, 30.36, 30.35, 30.46, 30.39, 30.39, 30…
#> $ soil_temp50        <dbl> 29.41, 29.41, 29.41, 29.43, 29.41, 29.43, 29.41, 29…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-07-11 20:00:00, 2026-07-11 19:45:00, 2026-07-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-07-10 20:20:00 UTC" "2026-07-11 20:00:00 UTC"
attr(recent$date_time, "tzone")
#> [1] "UTC"
```

## Retrieve one variable

TexMesonet also provides smaller single-variable endpoints. Use the
`variable` argument when you only need one series.

``` r

cache_file <- file.path(cache_dir, "tex_meso_time_series_blanco_temperature.rds")
if (!file.exists(cache_file)) {
  temperature <- tex_meso_time_series(
    site_id = site_id,
    prior_minutes = 24 * 60,
    variable = "temperature"
  )
  saveRDS(temperature, cache_file)
} else {
  temperature <- readRDS(cache_file)
}

temperature |>
  arrange(date_time) |>
  tail()
#> # A tibble: 6 × 2
#>   value date_time          
#>   <dbl> <dttm>             
#> 1    77 2026-07-11 19:45:00
#> 2    77 2026-07-11 19:50:00
#> 3    77 2026-07-11 19:55:00
#> 4    77 2026-07-11 20:00:00
#> 5    77 2026-07-11 20:05:00
#> 6    77 2026-07-11 20:10:00

attr(temperature, "units")
#> [1] "Fahrenheit"
```

Available single-variable values are `"temperature"`, `"humidity"`,
`"barometric_pressure"`, `"precip"`, and `"wind_speed"`.

## Example workflow: station weather summary

The charting-fields endpoint is useful when you want to prepare local
meteorological covariates for a stream site. The example below
summarizes hourly temperature, humidity, precipitation, and wind speed
from the recent station data.

``` r

hourly_weather <- recent |>
  mutate(hour = lubridate::floor_date(date_time, "hour")) |>
  group_by(hour) |>
  summarise(
    air_temp_C = mean(air_temp, na.rm = TRUE),
    humidity_pct = mean(humidity, na.rm = TRUE),
    precip_mm = sum(precip, na.rm = TRUE),
    wind_speed_m_s = mean(wind_speed, na.rm = TRUE),
    .groups = "drop"
  )

hourly_weather
#> # A tibble: 25 × 5
#>    hour                air_temp_C humidity_pct precip_mm wind_speed_m_s
#>    <dttm>                   <dbl>        <dbl>     <dbl>          <dbl>
#>  1 2026-07-10 20:00:00       31.8         44.8         0         3.19  
#>  2 2026-07-10 21:00:00       32.2         43.0         0         2.54  
#>  3 2026-07-10 22:00:00       31.3         47.6         0         2.81  
#>  4 2026-07-10 23:00:00       30.5         49.4         0         2.91  
#>  5 2026-07-11 00:00:00       30.2         46.1         0         2.33  
#>  6 2026-07-11 01:00:00       28.4         56.6         0         0.387 
#>  7 2026-07-11 02:00:00       26.8         60.5         0         0.200 
#>  8 2026-07-11 03:00:00       25.7         66.2         0         0     
#>  9 2026-07-11 04:00:00       25.3         70.4         0         0.112 
#> 10 2026-07-11 05:00:00       24.7         78.4         0         0.0733
#> # ℹ 15 more rows
```

Plotting the recent series can help identify gaps or station behavior
before combining meteorological data with stream logger observations.

``` r

ggplot(hourly_weather, aes(hour, air_temp_C)) +
  geom_line(color = "#2c7fb8") +
  geom_point(color = "#2c7fb8", size = 1.5) +
  labs(
    x = NULL,
    y = "Air temperature (°C)",
    title = "Recent TexMesonet air temperature"
  ) +
  theme_bw()
```

![](tex-mesonet_files/figure-html/plot-temperature-1.png)

## API scope

These functions use the lightweight TexMesonet API endpoints for TWDB
stations. For historical custom downloads, longer date ranges, or
non-TWDB provider networks shown in the TexMesonet map viewer, use the
TexMesonet Custom Downloads page or the source network recommended by
TWDB.
