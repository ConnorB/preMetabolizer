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
#> # A tibble: 123 × 6
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
#> # ℹ 113 more rows
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
#> Rows: 123
#> Columns: 39
#> $ object_id             <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_id            <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, …
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "RE…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 2206, 1881, 166, 3…
#> $ air_temp              <dbl> 27.88, 26.09, 26.45, 28.05, 23.12, 22.81, 32.97,…
#> $ air_temp2_m           <dbl> 27.88, 26.09, 26.45, 28.05, 23.12, 22.81, 32.97,…
#> $ humidity              <dbl> 72.21, 78.70, 81.40, 76.45, 81.80, 98.20, 41.74,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0,…
#> $ precip24_hr           <dbl> 5.588, 0.508, 1.270, 1.016, 0.762, 2.794, 0.000,…
#> $ precip48_hr           <dbl> 5.588, 0.508, 1.270, 1.016, 1.016, 2.794, 0.000,…
#> $ precip72_hr           <dbl> 5.842, 1.524, 5.842, 37.846, 6.096, 2.794, 0.000…
#> $ wind_speed            <dbl> 2.0707, 1.7271, 2.1064, 1.6964, 2.5036, 2.7271, …
#> $ wind_speed2_m         <dbl> 2.0707, 1.7271, 2.1064, 1.6964, 1.3801, 2.7271, …
#> $ wind_direction        <dbl> 114, 101, 80, 148, 223, 161, 157, 136, 174, 47, …
#> $ wind_direction2_m     <dbl> 114, 101, 80, 148, 53, 161, 112, 175, 174, 47, 3…
#> $ wind_gust             <dbl> 5.0734, 4.5408, 3.4756, 2.6767, 3.9429, 5.3397, …
#> $ wind_gust2_m          <dbl> 5.0734, 4.5408, 3.4756, 2.6767, 2.9430, 5.3397, …
#> $ battery_voltage       <dbl> 13.43, 13.49, 13.51, 13.01, 12.99, 13.29, 13.28,…
#> $ soil_moisture         <dbl> 0.265, 0.374, 0.266, 0.509, 0.225, 0.363, 0.113,…
#> $ soil_temperature      <dbl> 28.39, 24.52, 25.96, 25.86, 27.59, 26.19, 30.16,…
#> $ soil_moisture5_cm     <dbl> 0.192, 0.288, 0.183, 0.433, 0.097, 0.159, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 30.29, 26.71, 29.24, 34.41, 29.80, 28.89, NA, 26…
#> $ soil_moisture10_cm    <dbl> 0.213, 0.172, 0.284, 0.481, 0.233, 0.243, 0.121,…
#> $ soil_temperature10_cm <dbl> 30.19, 25.78, 28.42, 27.25, 29.98, 27.80, 34.05,…
#> $ soil_moisture20_cm    <dbl> 0.265, 0.374, 0.266, 0.509, 0.225, 0.363, 0.113,…
#> $ soil_temperature20_cm <dbl> 28.39, 24.52, 25.96, 25.86, 27.59, 26.19, 30.16,…
#> $ soil_moisture50_cm    <dbl> 0.268, NA, 0.298, 0.556, NA, NA, NA, 0.246, NA, …
#> $ soil_temperature50_cm <dbl> 25.87, NA, 24.85, NA, NA, NA, NA, 25.56, NA, 27.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-06-08 23:50:00, 2026-06-08 23:50:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 22.96, NA, 32.61, 29.47, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 2.5036, NA, 6.9908, 0.0295, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 223, NA, 157, 136, NA, NA, 79, 1…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 3.9429, NA, 9.7739, 0.4998, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 939.7231, NA, 945.5057, 1009.215…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 74.3613, NA, 146.6131, 206.2581,…
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
#> 1          2 Altwein Rd   2026-06-08 23:50:00     27.9     72.2      0
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
#> $ air_temp           <dbl> 27.75, 27.71, 27.42, 26.69, 25.58, 25.72, 25.04, 24…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 73.12, 70.63, 73.94, 75.83, 87.40, 90.40, 93.90, 96…
#> $ precip             <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.…
#> $ wind_speed         <dbl> 1.8518, 1.3853, 1.8230, 2.3250, 2.9606, 2.5490, 2.5…
#> $ wind_speed2_m      <dbl> 1.8518, 1.3853, 1.8230, 2.3250, 2.9606, 2.5490, 2.5…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 105, 127, 119, 103, 113, 176, 181, 154, 178, 165, 7…
#> $ wind_direction2_m  <dbl> 105, 127, 119, 103, 113, 176, 181, 154, 178, 165, 7…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 3.4756, 4.2745, 4.5408, 4.5408, 5.6060, 4.5408, 5.6…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.192, 0.193, 0.192, 0.193, 0.193, 0.193, 0.192, 0.…
#> $ soil_moist10       <dbl> 0.213, 0.213, 0.213, 0.213, 0.213, 0.213, 0.213, 0.…
#> $ soil_moist20       <dbl> 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.265, 0.…
#> $ soil_moist50       <dbl> 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.268, 0.…
#> $ soil_temp5         <dbl> 30.30, 30.39, 30.48, 30.58, 30.64, 30.73, 30.87, 31…
#> $ soil_temp10        <dbl> 30.20, 30.24, 30.28, 30.31, 30.38, 30.41, 30.48, 30…
#> $ soil_temp20        <dbl> 28.40, 28.35, 28.32, 28.27, 28.23, 28.17, 28.11, 28…
#> $ soil_temp50        <dbl> 25.88, 25.88, 25.89, 25.88, 25.89, 25.88, 25.86, 25…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-06-08 23:45:00, 2026-06-08 23:30:00, 2026-06-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-06-08 00:00:00 UTC" "2026-06-08 23:45:00 UTC"
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
#> 1    82 2026-06-08 23:25:00
#> 2    82 2026-06-08 23:30:00
#> 3    82 2026-06-08 23:35:00
#> 4    82 2026-06-08 23:40:00
#> 5    82 2026-06-08 23:45:00
#> 6    82 2026-06-08 23:50:00

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
#> # A tibble: 24 × 5
#>    hour                air_temp_C humidity_pct precip_mm wind_speed_m_s
#>    <dttm>                   <dbl>        <dbl>     <dbl>          <dbl>
#>  1 2026-06-08 00:00:00       29.3         63.3         0           3.17
#>  2 2026-06-08 01:00:00       27.7         70.2         0           1.87
#>  3 2026-06-08 02:00:00       26.4         82.9         0           1.78
#>  4 2026-06-08 03:00:00       25.7         86.6         0           1.94
#>  5 2026-06-08 04:00:00       25.2         89.4         0           2.74
#>  6 2026-06-08 05:00:00       24.6         89.9         0           3.58
#>  7 2026-06-08 06:00:00       23.9         89.7         0           3.73
#>  8 2026-06-08 07:00:00       23.4         88.1         0           3.92
#>  9 2026-06-08 08:00:00       23.2         90.2         0           3.85
#> 10 2026-06-08 09:00:00       23.2         90.4         0           3.14
#> # ℹ 14 more rows
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
