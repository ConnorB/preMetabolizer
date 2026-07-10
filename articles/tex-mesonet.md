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
#> Rows: 126
#> Columns: 39
#> $ object_id             <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_id            <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "RE…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 2206, 1881, 166, 3…
#> $ air_temp              <dbl> 32.86, 30.43, 31.20, 25.68, 31.74, 32.74, 32.90,…
#> $ air_temp2_m           <dbl> 32.86, 30.43, 31.20, 25.68, 31.74, 32.74, 32.90,…
#> $ humidity              <dbl> 44.23, 53.67, 49.49, 85.40, 43.89, 40.21, 37.23,…
#> $ precip                <dbl> 0.000, 0.000, 0.000, 0.254, 0.000, 0.000, 0.000,…
#> $ precip24_hr           <dbl> 0.000, 0.000, 0.000, 4.318, 0.000, 0.000, 0.000,…
#> $ precip48_hr           <dbl> 0.000, 0.000, 0.000, 4.318, 0.000, 0.000, 0.000,…
#> $ precip72_hr           <dbl> 0.000, 0.000, 0.000, 4.318, 9.398, 3.556, 0.000,…
#> $ wind_speed            <dbl> 2.6653, 3.5021, 2.8015, 1.5314, 5.4521, 2.0255, …
#> $ wind_speed2_m         <dbl> 2.6653, 3.5021, 2.8015, 1.5314, 3.5796, 2.0255, …
#> $ wind_direction        <dbl> 177, 150, 144, 195, 206, 144, 131, 141, 176, 139…
#> $ wind_direction2_m     <dbl> 177, 150, 144, 195, 51, 144, 131, 161, 176, 139,…
#> $ wind_gust             <dbl> 6.1386, 8.0027, 7.2038, 3.2093, 8.8853, 4.2745, …
#> $ wind_gust2_m          <dbl> 6.1386, 8.0027, 7.2038, 3.2093, 5.8723, 4.2745, …
#> $ battery_voltage       <dbl> 13.27, 13.37, 13.42, 13.50, 13.43, 13.32, 13.35,…
#> $ soil_moisture         <dbl> 0.213, 0.301, 0.199, 0.428, 0.194, 0.323, NA, 0.…
#> $ soil_temperature      <dbl> 31.23, 26.20, 28.76, 27.94, 29.58, 28.45, NA, 27…
#> $ soil_moisture5_cm     <dbl> 0.133, 0.194, 0.110, 0.204, 0.086, 0.108, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 35.20, 30.05, 37.72, 46.92, 35.44, 31.47, NA, 27…
#> $ soil_moisture10_cm    <dbl> 0.162, 0.124, 0.217, 0.373, 0.198, 0.204, NA, 0.…
#> $ soil_temperature10_cm <dbl> 34.07, 28.01, 32.26, 29.36, 31.91, 29.79, NA, 27…
#> $ soil_moisture20_cm    <dbl> 0.213, 0.301, 0.199, 0.428, 0.194, 0.323, NA, 0.…
#> $ soil_temperature20_cm <dbl> 31.23, 26.20, 28.76, 27.94, 29.58, 28.45, NA, 27…
#> $ soil_moisture50_cm    <dbl> 0.237, NA, 0.233, 0.520, NA, NA, NA, 0.176, NA, …
#> $ soil_temperature50_cm <dbl> 29.44, NA, 27.75, NA, NA, NA, NA, 27.10, NA, 28.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-07-10 22:25:00, 2026-07-10 22:25:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 30.65, NA, 31.63, 31.45, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 5.4521, NA, NA, 0.2021, NA, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 206, NA, NA, 141, NA, NA, 83, 14…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 8.8853, NA, NA, 1.7215, NA, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 939.9924, NA, 947.4011, 1009.083…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 586.7276, NA, 543.7220, 210.2085…
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
#> 1          2 Altwein Rd   2026-07-10 22:25:00     32.9     44.2      0
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
#> $ air_temp           <dbl> 32.18, 31.28, 32.70, 32.44, 31.72, 31.77, 31.16, 32…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 43.86, 45.65, 42.68, 42.04, 42.81, 44.43, 46.42, 41…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 3.4279, 2.1881, 1.8948, 3.2925, 3.0801, 1.8941, 2.7…
#> $ wind_speed2_m      <dbl> 3.4279, 2.1881, 1.8948, 3.2925, 3.0801, 1.8941, 2.7…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 140, 147, 138, 198, 89, 162, 146, 129, 164, 173, 16…
#> $ wind_direction2_m  <dbl> 140, 147, 138, 198, 89, 162, 146, 129, 164, 173, 16…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 6.4049, 6.9375, 5.6060, 6.6712, 5.0734, 5.3397, 5.8…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.133, 0.133, 0.133, 0.133, 0.133, 0.133, 0.133, 0.…
#> $ soil_moist10       <dbl> 0.162, 0.162, 0.162, 0.162, 0.161, 0.161, 0.161, 0.…
#> $ soil_moist20       <dbl> 0.213, 0.213, 0.213, 0.213, 0.213, 0.213, 0.214, 0.…
#> $ soil_moist50       <dbl> 0.237, 0.237, 0.237, 0.237, 0.237, 0.237, 0.237, 0.…
#> $ soil_temp5         <dbl> 35.17, 35.09, 35.01, 34.96, 34.85, 34.72, 34.56, 34…
#> $ soil_temp10        <dbl> 33.99, 33.85, 33.70, 33.56, 33.42, 33.23, 33.07, 32…
#> $ soil_temp20        <dbl> 31.17, 31.12, 31.07, 31.03, 30.96, 30.93, 30.87, 30…
#> $ soil_temp50        <dbl> 29.42, 29.41, 29.46, 29.44, 29.48, 29.47, 29.41, 29…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-07-10 22:15:00, 2026-07-10 22:00:00, 2026-07-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-07-09 22:30:00 UTC" "2026-07-10 22:15:00 UTC"
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
#> 1    88 2026-07-10 22:00:00
#> 2    89 2026-07-10 22:05:00
#> 3    90 2026-07-10 22:10:00
#> 4    90 2026-07-10 22:15:00
#> 5    91 2026-07-10 22:20:00
#> 6    91 2026-07-10 22:25:00

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
#>  1 2026-07-09 22:00:00       32.5         37.1         0          1.40 
#>  2 2026-07-09 23:00:00       32.5         35.8         0          2.49 
#>  3 2026-07-10 00:00:00       31.5         39.7         0          2.54 
#>  4 2026-07-10 01:00:00       29.8         45.0         0          1.79 
#>  5 2026-07-10 02:00:00       28.2         52.6         0          2.30 
#>  6 2026-07-10 03:00:00       27.0         57.4         0          2.00 
#>  7 2026-07-10 04:00:00       26.0         62.5         0          2.16 
#>  8 2026-07-10 05:00:00       24.8         69.2         0          1.39 
#>  9 2026-07-10 06:00:00       24.1         69.2         0          1.93 
#> 10 2026-07-10 07:00:00       23           71.8         0          0.826
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
