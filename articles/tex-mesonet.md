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
#> $ air_temp              <dbl> 26.27, 25.78, 27.00, 27.15, 26.32, 26.83, 28.75,…
#> $ air_temp2_m           <dbl> 26.27, 25.78, 27.00, 27.15, 26.32, 26.83, 28.75,…
#> $ humidity              <dbl> 76.71, 79.89, 74.52, 77.53, 71.97, 70.95, 53.36,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0,…
#> $ precip24_hr           <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,…
#> $ precip48_hr           <dbl> 0.000, 0.000, 0.000, 0.000, 0.508, 0.762, 0.000,…
#> $ precip72_hr           <dbl> 5.588, 0.508, 1.270, 1.016, 1.270, 3.556, 0.000,…
#> $ wind_speed            <dbl> 1.9228, 1.9042, 1.6308, 1.5553, 4.3505, 2.2043, …
#> $ wind_speed2_m         <dbl> 1.9228, 1.9042, 1.6308, 1.5553, 2.7431, 2.2043, …
#> $ wind_direction        <dbl> 153, 139, 118, 181, 176, 197, 167, 121, 141, 128…
#> $ wind_direction2_m     <dbl> 153, 139, 118, 181, 51, 197, 128, 154, 141, 128,…
#> $ wind_gust             <dbl> 3.4756, 3.7419, 4.5408, 3.2093, 6.5529, 4.8071, …
#> $ wind_gust2_m          <dbl> 3.4756, 3.7419, 4.5408, 3.2093, 4.8071, 4.8071, …
#> $ battery_voltage       <dbl> 12.74, 12.76, 12.69, 12.73, 12.78, 12.89, 13.12,…
#> $ soil_moisture         <dbl> 0.257, 0.358, 0.260, 0.502, 0.213, 0.356, 0.110,…
#> $ soil_temperature      <dbl> 28.98, 24.98, 26.46, 26.33, 28.15, 26.28, 30.80,…
#> $ soil_moisture5_cm     <dbl> 0.192, 0.239, 0.153, 0.423, 0.086, 0.146, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 30.44, 25.93, 28.27, 34.72, 29.18, 27.23, NA, 26…
#> $ soil_moisture10_cm    <dbl> 0.206, 0.157, 0.266, 0.474, 0.223, 0.237, 0.114,…
#> $ soil_temperature10_cm <dbl> 30.83, 25.81, 28.60, 27.82, 30.00, 27.12, 32.64,…
#> $ soil_moisture20_cm    <dbl> 0.257, 0.358, 0.260, 0.502, 0.213, 0.356, 0.110,…
#> $ soil_temperature20_cm <dbl> 28.98, 24.98, 26.46, 26.33, 28.15, 26.28, 30.80,…
#> $ soil_moisture50_cm    <dbl> 0.262, NA, 0.290, 0.547, NA, NA, NA, 0.238, NA, …
#> $ soil_temperature50_cm <dbl> 26.07, NA, 25.18, NA, NA, NA, NA, 25.83, NA, 27.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-06-11 03:00:00, 2026-06-11 03:00:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 26.55, NA, 29.12, 27.68, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 4.3505, NA, 5.3307, 0.0309, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 176, NA, 167, 121, NA, NA, 128, …
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 6.5529, NA, 7.1083, 0.6664, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 936.7719, NA, 943.7466, 1006.405…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 0, NA, 0, 0, NA, NA, 0, 0, 0, 0,…
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
#> 1          2 Altwein Rd   2026-06-11 03:00:00     26.3     76.7      0
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
#> Rows: 97
#> Columns: 27
#> $ air_temp           <dbl> 26.27, 26.45, 26.78, 27.01, 27.34, 27.50, 27.86, 28…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 76.71, 75.29, 73.07, 71.52, 70.16, 71.07, 69.35, 68…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 1.9228, 0.3474, 1.7210, 0.1291, 1.8447, 1.7723, 2.7…
#> $ wind_speed2_m      <dbl> 1.9228, 0.3474, 1.7210, 0.1291, 1.8447, 1.7723, 2.7…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 153, 132, 108, 162, 174, 137, 162, 169, 156, 234, 1…
#> $ wind_direction2_m  <dbl> 153, 132, 108, 162, 174, 137, 162, 169, 156, 234, 1…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 3.4756, 4.0082, 3.7419, 2.9430, 3.7419, 3.4756, 5.8…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.192, 0.192, 0.192, 0.192, 0.192, 0.192, 0.193, 0.…
#> $ soil_moist10       <dbl> 0.206, 0.206, 0.206, 0.206, 0.206, 0.206, 0.206, 0.…
#> $ soil_moist20       <dbl> 0.257, 0.257, 0.257, 0.257, 0.258, 0.258, 0.258, 0.…
#> $ soil_moist50       <dbl> 0.262, 0.262, 0.262, 0.262, 0.263, 0.263, 0.263, 0.…
#> $ soil_temp5         <dbl> 30.44, 30.57, 30.71, 30.88, 31.03, 31.19, 31.33, 31…
#> $ soil_temp10        <dbl> 30.83, 30.93, 31.03, 31.12, 31.18, 31.31, 31.37, 31…
#> $ soil_temp20        <dbl> 28.98, 28.96, 28.91, 28.85, 28.83, 28.79, 28.75, 28…
#> $ soil_temp50        <dbl> 26.07, 26.11, 26.06, 26.07, 26.12, 26.07, 26.09, 26…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-06-11 03:00:00, 2026-06-11 02:45:00, 2026-06-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-06-10 03:05:00 UTC" "2026-06-11 03:00:00 UTC"
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
#> 1    80 2026-06-11 02:35:00
#> 2    80 2026-06-11 02:40:00
#> 3    80 2026-06-11 02:45:00
#> 4    80 2026-06-11 02:50:00
#> 5    79 2026-06-11 02:55:00
#> 6    79 2026-06-11 03:00:00

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
#>  1 2026-06-10 03:00:00       24.8         76.9         0         0.646 
#>  2 2026-06-10 04:00:00       24.1         75.3         0         1.89  
#>  3 2026-06-10 05:00:00       23.2         80.3         0         1.89  
#>  4 2026-06-10 06:00:00       22.7         87.3         0         2.71  
#>  5 2026-06-10 07:00:00       22.1         89.1         0         2.72  
#>  6 2026-06-10 08:00:00       22.0         91.1         0         1.76  
#>  7 2026-06-10 09:00:00       21.9         93.0         0         0.636 
#>  8 2026-06-10 10:00:00       22.0         94.1         0         0.0223
#>  9 2026-06-10 11:00:00       22.0         94.0         0         0.0180
#> 10 2026-06-10 12:00:00       22.1         93.6         0         0.0706
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
