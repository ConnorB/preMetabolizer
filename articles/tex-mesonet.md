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
#> $ air_temp              <dbl> 25.45, 24.43, 26.02, 26.49, 20.56, 19.62, 26.27,…
#> $ air_temp2_m           <dbl> 25.45, 24.43, 26.02, 26.49, 20.56, 19.62, 26.27,…
#> $ humidity              <dbl> 73.98, 80.20, 74.66, 77.09, 83.00, 85.50, 65.40,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ precip24_hr           <dbl> 49.784, 24.638, 22.352, 25.146, 73.152, 38.354, …
#> $ precip48_hr           <dbl> 49.784, 24.638, 22.352, 25.146, 73.152, 38.354, …
#> $ precip72_hr           <dbl> 49.784, 24.892, 23.114, 26.162, 73.660, 38.354, …
#> $ wind_speed            <dbl> 0.8579, 0.5620, 1.3887, 1.5194, 2.4944, 0.0072, …
#> $ wind_speed2_m         <dbl> 0.8579, 0.5620, 1.3887, 1.5194, 1.7074, 0.0072, …
#> $ wind_direction        <dbl> 167, 82, 156, 116, 136, 155, 104, 50, 61, 23, 31…
#> $ wind_direction2_m     <dbl> 167, 82, 156, 116, 70, 155, 104, 95, 61, 23, 321…
#> $ wind_gust             <dbl> 4.0082, 2.1441, 2.9430, 2.9430, 3.2765, 0.3596, …
#> $ wind_gust2_m          <dbl> 4.0082, 2.1441, 2.9430, 2.9430, 2.6767, 0.3596, …
#> $ battery_voltage       <dbl> 13.50, 13.62, 13.64, 13.57, 14.25, 13.40, 13.57,…
#> $ soil_moisture         <dbl> 0.325, 0.396, 0.201, 0.516, 0.452, 0.561, NA, 0.…
#> $ soil_temperature      <dbl> 28.54, 25.42, 27.89, 27.06, 26.77, 26.15, NA, 26…
#> $ soil_moisture5_cm     <dbl> 0.354, 0.363, 0.366, 0.422, 0.321, 0.246, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 28.30, 26.06, 28.83, 34.71, 24.97, 24.79, NA, 26…
#> $ soil_moisture10_cm    <dbl> 0.350, 0.276, 0.419, 0.486, 0.484, 0.375, NA, 0.…
#> $ soil_temperature10_cm <dbl> 28.25, 25.55, 28.11, 27.34, 25.55, 25.29, NA, 26…
#> $ soil_moisture20_cm    <dbl> 0.325, 0.396, 0.201, 0.516, 0.452, 0.561, NA, 0.…
#> $ soil_temperature20_cm <dbl> 28.54, 25.42, 27.89, 27.06, 26.77, 26.15, NA, 26…
#> $ soil_moisture50_cm    <dbl> 0.243, NA, 0.235, 0.514, NA, NA, NA, 0.169, NA, …
#> $ soil_temperature50_cm <dbl> 28.69, NA, 27.49, NA, NA, NA, NA, 26.54, NA, 28.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-07-14 21:05:00, 2026-07-14 21:05:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 20.22, NA, 26.04, 25.00, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 2.4944, NA, NA, 0.5284, NA, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 136, NA, NA, 50, NA, NA, 316, 61…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 3.2765, NA, NA, 1.6660, NA, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 942.8373, NA, 950.3366, 1012.286…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 216.9576, NA, 492.6425, 179.3345…
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
#> 1          2 Altwein Rd   2026-07-14 21:05:00     25.4     74.0      0
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
#> Rows: 97
#> Columns: 27
#> $ air_temp           <dbl> 25.64, 25.59, 25.53, 25.45, 24.85, 24.96, 24.57, 24…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 73.50, 71.75, 70.30, 70.03, 74.11, 71.82, 73.37, 70…
#> $ precip             <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.…
#> $ wind_speed         <dbl> 0.5223, 0.9396, 1.8662, 0.0000, 1.6758, 0.7092, 0.4…
#> $ wind_speed2_m      <dbl> 0.5223, 0.9396, 1.8662, 0.0000, 1.6758, 0.7092, 0.4…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 139, 98, 114, 79, 170, 165, 140, 167, 174, 188, 169…
#> $ wind_direction2_m  <dbl> 139, 98, 114, 79, 170, 165, 140, 167, 174, 188, 169…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 2.9430, 3.4756, 4.2745, 0.0000, 3.4756, 3.7419, 2.9…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.354, 0.355, 0.356, 0.357, 0.357, 0.358, 0.358, 0.…
#> $ soil_moist10       <dbl> 0.350, 0.351, 0.352, 0.352, 0.353, 0.353, 0.355, 0.…
#> $ soil_moist20       <dbl> 0.325, 0.326, 0.326, 0.326, 0.327, 0.327, 0.327, 0.…
#> $ soil_moist50       <dbl> 0.243, 0.243, 0.243, 0.243, 0.243, 0.243, 0.243, 0.…
#> $ soil_temp5         <dbl> 28.27, 28.21, 28.15, 28.01, 27.95, 27.92, 27.84, 27…
#> $ soil_temp10        <dbl> 28.21, 28.16, 28.11, 28.02, 27.98, 27.94, 27.92, 27…
#> $ soil_temp20        <dbl> 28.52, 28.52, 28.48, 28.52, 28.53, 28.53, 28.52, 28…
#> $ soil_temp50        <dbl> 28.71, 28.71, 28.74, 28.73, 28.71, 28.71, 28.73, 28…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-07-14 21:00:00, 2026-07-14 20:45:00, 2026-07-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-07-13 21:10:00 UTC" "2026-07-14 21:00:00 UTC"
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
#> 1    78 2026-07-14 20:40:00
#> 2    78 2026-07-14 20:45:00
#> 3    78 2026-07-14 20:50:00
#> 4    78 2026-07-14 20:55:00
#> 5    78 2026-07-14 21:00:00
#> 6    78 2026-07-14 21:05:00

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
#>  1 2026-07-13 21:00:00       31.7         48.4     0            1.15   
#>  2 2026-07-13 22:00:00       32.2         43.8     0            1.01   
#>  3 2026-07-13 23:00:00       30.3         56.0     0            0.350  
#>  4 2026-07-14 00:00:00       24.3         87.7    23.6          1.26   
#>  5 2026-07-14 01:00:00       22.2         99.8     9.65         0.0123 
#>  6 2026-07-14 02:00:00       22.3         99.9     4.32         0      
#>  7 2026-07-14 03:00:00       22.2         99.9     5.33         0      
#>  8 2026-07-14 04:00:00       22.4         99.9     2.03         0.00988
#>  9 2026-07-14 05:00:00       22.9         99.9     0.508        0.0725 
#> 10 2026-07-14 06:00:00       23.2         99.9     0            0      
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
