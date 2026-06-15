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
#> Rows: 123
#> Columns: 39
#> $ object_id             <int> 2, 3, 4, 5, 6, 7, 9, 10, 13, 14, 15, 16, 17, 18,…
#> $ station_id            <int> 2, 3, 4, 5, 6, 7, 9, 10, 13, 14, 15, 16, 17, 18,…
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "RE…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 2206, 166, 339, 48…
#> $ air_temp              <dbl> 26.92, 25.57, 25.53, 27.70, 22.11, 25.55, 25.10,…
#> $ air_temp2_m           <dbl> 26.92, 25.57, 25.53, 27.70, 22.11, 25.55, 25.10,…
#> $ humidity              <dbl> 68.13, 77.27, 77.54, 69.04, 91.00, 74.30, 92.00,…
#> $ precip                <dbl> 0.000, 0.000, 0.000, 0.000, 0.254, 0.000, NA, 0.…
#> $ precip24_hr           <dbl> 90.424, 63.246, 48.006, 59.182, 149.352, 17.018,…
#> $ precip48_hr           <dbl> 90.678, 63.246, 48.006, 59.182, 149.352, 17.018,…
#> $ precip72_hr           <dbl> 90.678, 63.246, 48.006, 59.182, 149.352, 17.018,…
#> $ wind_speed            <dbl> 0.0000, 0.8644, 0.0000, 0.8276, 0.7878, 1.1239, …
#> $ wind_speed2_m         <dbl> 0.0000, 0.8644, 0.0000, 0.8276, 0.9225, 1.1239, …
#> $ wind_direction        <dbl> 8, 134, 118, 193, 163, 141, 140, 324, 347, 78, 4…
#> $ wind_direction2_m     <dbl> 8, 134, 118, 193, 77, 141, 165, 324, 347, 98, 35…
#> $ wind_gust             <dbl> 0.0000, 4.0082, 0.0000, 1.6115, 2.3879, 2.6767, …
#> $ wind_gust2_m          <dbl> 0.0000, 4.0082, 0.0000, 1.6115, 1.8778, 2.6767, …
#> $ battery_voltage       <dbl> 13.45, 13.50, 14.06, 13.97, 13.75, 13.99, 13.87,…
#> $ soil_moisture         <dbl> 0.363, 0.464, 0.290, 0.594, 0.433, 0.340, 0.108,…
#> $ soil_temperature      <dbl> 25.77, 23.68, 26.48, 25.92, 24.91, 26.30, 26.37,…
#> $ soil_moisture5_cm     <dbl> 0.367, 0.432, 0.381, 0.618, 0.330, 0.177, 0.219,…
#> $ soil_temperature5_cm  <dbl> 26.46, 25.12, 26.28, 30.61, 25.49, 26.65, 26.29,…
#> $ soil_moisture10_cm    <dbl> 0.354, 0.359, 0.446, 0.596, 0.469, 0.242, 0.135,…
#> $ soil_temperature10_cm <dbl> 25.71, 24.03, 25.82, 25.95, 24.83, 26.41, 26.28,…
#> $ soil_moisture20_cm    <dbl> 0.363, 0.464, 0.290, 0.594, 0.433, 0.340, 0.108,…
#> $ soil_temperature20_cm <dbl> 25.77, 23.68, 26.48, 25.92, 24.91, 26.30, 26.37,…
#> $ soil_moisture50_cm    <dbl> 0.349, NA, 0.271, 0.632, NA, NA, 0.220, NA, 0.39…
#> $ soil_temperature50_cm <dbl> 27.09, NA, 25.95, NA, NA, NA, 26.15, NA, 27.80, …
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-06-15 19:25:00, 2026-06-15 19:25:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 21.96, NA, 26.05, NA, NA, 23.38,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 0.7878, NA, 0.0134, NA, NA, 0.47…
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 163, NA, 140, NA, NA, 78, 4, 32,…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 2.3879, NA, 0.4998, NA, NA, 1.59…
#> $ air_pressure          <dbl> NA, NA, NA, NA, 940.9660, NA, 1008.2140, NA, NA,…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 111.6767, NA, 149.4883, NA, NA, …
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
#> 1          2 Altwein Rd   2026-06-15 19:25:00     26.9     68.1      0
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
#> $ air_temp           <dbl> 27.19, 27.14, 27.04, 26.61, 26.41, 25.92, 25.24, 24…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 65.17, 67.05, 71.84, 69.31, 70.46, 71.04, 75.79, 78…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0830, 0.0…
#> $ wind_speed2_m      <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0830, 0.0…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 178, 218, 122, 139, 79, 178, 183, 36, 122, 39, 266,…
#> $ wind_direction2_m  <dbl> 178, 218, 122, 139, 79, 178, 183, 36, 122, 39, 266,…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 2.1441, 0.0…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.368, 0.368, 0.369, 0.370, 0.371, 0.372, 0.373, 0.…
#> $ soil_moist10       <dbl> 0.355, 0.355, 0.356, 0.357, 0.358, 0.359, 0.360, 0.…
#> $ soil_moist20       <dbl> 0.363, 0.364, 0.365, 0.366, 0.366, 0.367, 0.368, 0.…
#> $ soil_moist50       <dbl> 0.349, 0.350, 0.350, 0.351, 0.352, 0.352, 0.353, 0.…
#> $ soil_temp5         <dbl> 26.30, 26.07, 25.84, 25.60, 25.35, 25.16, 24.99, 24…
#> $ soil_temp10        <dbl> 25.62, 25.44, 25.28, 25.13, 25.00, 24.86, 24.72, 24…
#> $ soil_temp20        <dbl> 25.76, 25.73, 25.68, 25.66, 25.65, 25.61, 25.59, 25…
#> $ soil_temp50        <dbl> 27.12, 27.09, 27.09, 27.09, 27.12, 27.10, 27.11, 27…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-06-15 19:15:00, 2026-06-15 19:00:00, 2026-06-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-06-14 19:35:00 UTC" "2026-06-15 19:15:00 UTC"
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
#> 1    81 2026-06-15 19:00:00
#> 2    81 2026-06-15 19:05:00
#> 3    81 2026-06-15 19:10:00
#> 4    81 2026-06-15 19:15:00
#> 5    81 2026-06-15 19:20:00
#> 6    80 2026-06-15 19:25:00

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
#>  1 2026-06-14 19:00:00       31.7         57.4         0          0.936
#>  2 2026-06-14 20:00:00       31.5         55.3         0          1.37 
#>  3 2026-06-14 21:00:00       32.3         52.9         0          1.03 
#>  4 2026-06-14 22:00:00       31.9         56.2         0          0.482
#>  5 2026-06-14 23:00:00       31.5         55.5         0          0.724
#>  6 2026-06-15 00:00:00       29.4         68.0         0          1.24 
#>  7 2026-06-15 01:00:00       27.4         76.3         0          0.209
#>  8 2026-06-15 02:00:00       26.2         81.5         0          0    
#>  9 2026-06-15 03:00:00       25.3         86.2         0          0    
#> 10 2026-06-15 04:00:00       25.1         89.0         0          0.551
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
