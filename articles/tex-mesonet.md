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
- [`tex_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_timeseries.md)
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
#> Rows: 137
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
#> # A tibble: 124 × 6
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
#> # ℹ 114 more rows
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
#> Rows: 124
#> Columns: 39
#> $ object_id             <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_id            <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "RE…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 2206, 1881, 166, 3…
#> $ air_temp              <dbl> 31.49, 30.42, 31.82, 32.90, 31.21, 30.73, 32.86,…
#> $ air_temp2_m           <dbl> 31.49, 30.42, 31.82, 32.90, 31.21, 30.73, 32.86,…
#> $ humidity              <dbl> 50.06, 55.22, 51.52, 55.03, 48.53, 48.80, 40.39,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0,…
#> $ precip24_hr           <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,…
#> $ precip48_hr           <dbl> 0.000, 0.000, 0.000, 0.000, 1.016, 3.556, 0.000,…
#> $ precip72_hr           <dbl> 5.588, 0.508, 1.270, 1.016, 1.270, 3.556, 0.000,…
#> $ wind_speed            <dbl> 3.4329, 2.9103, 3.2651, 2.3078, 6.7102, 2.9925, …
#> $ wind_speed2_m         <dbl> 3.4329, 2.9103, 3.2651, 2.3078, 3.9514, 2.9925, …
#> $ wind_direction        <dbl> 176, 182, 178, 149, 246, 128, 205, 157, 163, 130…
#> $ wind_direction2_m     <dbl> 176, 182, 178, 149, 50, 128, 154, 196, 163, 130,…
#> $ wind_gust             <dbl> 6.1386, 5.0734, 5.8723, 4.5408, 9.9405, 6.9375, …
#> $ wind_gust2_m          <dbl> 6.1386, 5.0734, 5.8723, 4.5408, 7.2038, 6.9375, …
#> $ battery_voltage       <dbl> 13.26, 13.40, 13.45, 13.33, 13.48, 13.38, 13.30,…
#> $ soil_moisture         <dbl> 0.258, 0.360, 0.260, 0.502, 0.212, 0.355, 0.109,…
#> $ soil_temperature      <dbl> 27.57, 24.42, 26.04, 25.87, 26.94, 25.68, 29.67,…
#> $ soil_moisture5_cm     <dbl> 0.197, 0.248, 0.159, 0.424, 0.093, 0.149, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 32.18, 27.64, 32.48, 34.65, 34.12, 28.70, NA, 27…
#> $ soil_moisture10_cm    <dbl> 0.208, 0.159, 0.268, 0.473, 0.225, 0.236, 0.117,…
#> $ soil_temperature10_cm <dbl> 30.78, 25.88, 29.05, 27.21, 29.70, 26.89, 33.72,…
#> $ soil_moisture20_cm    <dbl> 0.258, 0.360, 0.260, 0.502, 0.212, 0.355, 0.109,…
#> $ soil_temperature20_cm <dbl> 27.57, 24.42, 26.04, 25.87, 26.94, 25.68, 29.67,…
#> $ soil_moisture50_cm    <dbl> 0.263, NA, 0.291, 0.548, NA, NA, NA, 0.239, NA, …
#> $ soil_temperature50_cm <dbl> 26.12, NA, 25.13, NA, NA, NA, NA, 25.72, NA, 27.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-06-10 21:25:00, 2026-06-10 21:25:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 29.98, NA, 31.62, 30.93, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 6.7102, NA, 8.7561, 0.0471, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 246, NA, 205, 157, NA, NA, 136, …
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 9.9405, NA, 12.4395, 0.4443, NA,…
#> $ air_pressure          <dbl> NA, NA, NA, NA, 937.3382, NA, 944.1740, 1006.592…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 838.9671, NA, 824.4895, 715.1444…
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
#> 1          2 Altwein Rd   2026-06-10 21:25:00     31.5     50.1      0
#> # ℹ 2 more variables: wind_speed <dbl>, air_pressure <dbl>
```

## Retrieve recent time series

[`tex_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_timeseries.md)
retrieves observations for a single station over a look-back window
measured in minutes. By default, `variable = "all"` uses the TexMesonet
charting-fields endpoint and returns all fields available for that
station.

``` r

cache_file <- file.path(cache_dir, "tex_meso_timeseries_blanco.rds")
if (!file.exists(cache_file)) {
  recent <- tex_meso_timeseries(
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
#> $ air_temp           <dbl> 31.59, 32.01, 31.63, 31.31, 31.85, 31.95, 30.74, 30…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 49.32, 49.02, 48.71, 48.65, 47.80, 48.34, 50.67, 51…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 3.6481, 3.3243, 3.5032, 2.9076, 3.6950, 3.3960, 3.4…
#> $ wind_speed2_m      <dbl> 3.6481, 3.3243, 3.5032, 2.9076, 3.6950, 3.3960, 3.4…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 177, 194, 162, 146, 197, 160, 170, 160, 161, 215, 3…
#> $ wind_direction2_m  <dbl> 177, 194, 162, 146, 197, 160, 170, 160, 161, 215, 3…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 7.4701, 6.1386, 8.0027, 6.4049, 6.4049, 6.1386, 6.1…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.197, 0.197, 0.197, 0.197, 0.197, 0.197, 0.197, 0.…
#> $ soil_moist10       <dbl> 0.208, 0.208, 0.208, 0.208, 0.208, 0.208, 0.207, 0.…
#> $ soil_moist20       <dbl> 0.258, 0.258, 0.258, 0.259, 0.259, 0.259, 0.259, 0.…
#> $ soil_moist50       <dbl> 0.264, 0.264, 0.264, 0.264, 0.264, 0.264, 0.264, 0.…
#> $ soil_temp5         <dbl> 32.06, 31.98, 31.84, 31.67, 31.47, 31.32, 31.14, 30…
#> $ soil_temp10        <dbl> 30.65, 30.44, 30.22, 29.99, 29.78, 29.56, 29.32, 29…
#> $ soil_temp20        <dbl> 27.54, 27.42, 27.42, 27.36, 27.33, 27.24, 27.22, 27…
#> $ soil_temp50        <dbl> 26.11, 26.13, 26.10, 26.12, 26.10, 26.13, 26.13, 26…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-06-10 21:15:00, 2026-06-10 21:00:00, 2026-06-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-06-09 21:35:00 UTC" "2026-06-10 21:15:00 UTC"
attr(recent$date_time, "tzone")
#> [1] "UTC"
```

## Retrieve one variable

TexMesonet also provides smaller single-variable endpoints. Use the
`variable` argument when you only need one series.

``` r

cache_file <- file.path(cache_dir, "tex_meso_timeseries_blanco_temperature.rds")
if (!file.exists(cache_file)) {
  temperature <- tex_meso_timeseries(
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
#> 1    90 2026-06-10 21:00:00
#> 2    90 2026-06-10 21:05:00
#> 3    90 2026-06-10 21:10:00
#> 4    89 2026-06-10 21:15:00
#> 5    89 2026-06-10 21:20:00
#> 6    89 2026-06-10 21:25:00

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
#>  1 2026-06-09 21:00:00       30.9         59.1         0         2.12  
#>  2 2026-06-09 22:00:00       30.9         58.1         0         1.81  
#>  3 2026-06-09 23:00:00       29.7         59.3         0         1.63  
#>  4 2026-06-10 00:00:00       28.9         60.5         0         1.25  
#>  5 2026-06-10 01:00:00       27.4         64.2         0         0.0482
#>  6 2026-06-10 02:00:00       25.9         71.9         0         0.0009
#>  7 2026-06-10 03:00:00       24.8         76.7         0         0.646 
#>  8 2026-06-10 04:00:00       24.1         75.3         0         1.89  
#>  9 2026-06-10 05:00:00       23.2         80.3         0         1.89  
#> 10 2026-06-10 06:00:00       22.7         87.3         0         2.71  
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
