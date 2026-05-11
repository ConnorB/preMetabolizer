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
#> $ air_temp              <dbl> 17.52, 17.41, 18.47, 18.35, 16.24, 16.55, 19.77,…
#> $ air_temp2_m           <dbl> 17.52, 17.41, 18.47, 18.35, 16.24, 16.55, 19.77,…
#> $ humidity              <dbl> 96.10, 96.20, 91.30, 94.30, 91.60, 91.70, 69.34,…
#> $ precip                <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,…
#> $ precip24_hr           <dbl> 1.016, 1.270, 4.572, 5.334, 8.636, 10.922, 0.254…
#> $ precip48_hr           <dbl> 1.524, 1.524, 5.588, 6.858, 8.890, 10.922, 0.254…
#> $ precip72_hr           <dbl> 1.524, 1.524, 5.588, 6.858, 9.398, 10.922, 0.254…
#> $ wind_speed            <dbl> 0.8327, 2.9368, 3.0091, 2.0290, 6.3151, 3.2696, …
#> $ wind_speed2_m         <dbl> 0.8327, 2.9368, 3.0091, 2.0290, 3.3649, 3.2696, …
#> $ wind_direction        <dbl> 294, 32, 28, 345, 123, 182, 32, 75, 177, 350, 30…
#> $ wind_direction2_m     <dbl> 294, 32, 28, 345, 126, 182, 9, 14, 177, 350, 322…
#> $ wind_gust             <dbl> 2.4104, 5.8723, 5.6060, 4.5408, 11.1622, 7.3813,…
#> $ wind_gust2_m          <dbl> 2.4104, 5.8723, 5.6060, 4.5408, 7.2038, 7.3813, …
#> $ battery_voltage       <dbl> 12.69, 12.71, 12.70, 12.60, 12.68, 12.35, 13.07,…
#> $ soil_moisture         <dbl> 0.256, 0.382, 0.397, 0.486, 0.315, 0.515, 0.165,…
#> $ soil_temperature      <dbl> 26.21, 22.01, 23.11, 22.73, 24.59, 23.29, 27.14,…
#> $ soil_moisture5_cm     <dbl> 0.210, 0.250, 0.301, 0.373, 0.205, 0.308, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 27.10, 22.31, 23.69, 32.14, 22.37, 23.70, NA, 24…
#> $ soil_moisture10_cm    <dbl> 0.222, 0.178, 0.315, 0.449, 0.296, 0.381, 0.152,…
#> $ soil_temperature10_cm <dbl> 27.88, 22.73, 24.93, 24.26, 25.25, 24.20, 27.70,…
#> $ soil_moisture20_cm    <dbl> 0.256, 0.382, 0.397, 0.486, 0.315, 0.515, 0.165,…
#> $ soil_temperature20_cm <dbl> 26.21, 22.01, 23.11, 22.73, 24.59, 23.29, 27.14,…
#> $ soil_moisture50_cm    <dbl> 0.241, NA, 0.383, 0.528, NA, NA, NA, 0.267, NA, …
#> $ soil_temperature50_cm <dbl> 22.59, NA, 21.97, NA, NA, NA, NA, 22.99, NA, 24.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-05-11 04:10:00, 2026-05-11 04:10:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 16.33, NA, 19.90, 25.12, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 6.3151, NA, 8.6172, 0.0591, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 123, NA, 32, 75, NA, NA, 307, 7,…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 11.1622, NA, 12.0507, 1.2217, NA…
#> $ air_pressure          <dbl> NA, NA, NA, NA, 942.4246, NA, 951.0060, 1010.524…
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
#> 1          2 Altwein Rd   2026-05-11 04:10:00     17.5     96.1      0
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
#> $ air_temp           <dbl> 17.58, 17.73, 17.78, 17.52, 17.37, 17.15, 16.66, 18…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 95.80, 95.50, 94.30, 95.00, 94.70, 94.40, 95.10, 85…
#> $ precip             <dbl> 0.254, 0.254, 0.000, 0.000, 0.000, 0.000, 0.254, 0.…
#> $ wind_speed         <dbl> 1.7318, 1.6950, 3.1065, 2.9952, 2.6943, 3.9295, 3.8…
#> $ wind_speed2_m      <dbl> 1.7318, 1.6950, 3.1065, 2.9952, 2.6943, 3.9295, 3.8…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 319, 294, 265, 307, 216, 348, 302, 286, 305, 266, 3…
#> $ wind_direction2_m  <dbl> 319, 294, 265, 307, 216, 348, 302, 286, 305, 266, 3…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 4.2745, 4.0082, 8.2690, 5.6060, 7.2038, 9.0679, 8.0…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.210, 0.210, 0.210, 0.210, 0.211, 0.212, 0.212, 0.…
#> $ soil_moist10       <dbl> 0.222, 0.222, 0.223, 0.222, 0.222, 0.223, 0.223, 0.…
#> $ soil_moist20       <dbl> 0.256, 0.256, 0.256, 0.256, 0.256, 0.256, 0.257, 0.…
#> $ soil_moist50       <dbl> 0.241, 0.240, 0.241, 0.241, 0.241, 0.241, 0.240, 0.…
#> $ soil_temp5         <dbl> 27.27, 27.48, 27.68, 27.95, 28.22, 28.44, 28.67, 28…
#> $ soil_temp10        <dbl> 27.97, 28.13, 28.29, 28.47, 28.62, 28.77, 28.89, 28…
#> $ soil_temp20        <dbl> 26.27, 26.25, 26.23, 26.22, 26.19, 26.14, 26.08, 26…
#> $ soil_temp50        <dbl> 22.57, 22.57, 22.57, 22.57, 22.54, 22.54, 22.53, 22…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-05-11 04:00:00, 2026-05-11 03:45:00, 2026-05-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-05-10 04:20:00 UTC" "2026-05-11 04:00:00 UTC"
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
#> 1    64 2026-05-11 03:45:00
#> 2    64 2026-05-11 03:50:00
#> 3    64 2026-05-11 03:55:00
#> 4    64 2026-05-11 04:00:00
#> 5    64 2026-05-11 04:05:00
#> 6    64 2026-05-11 04:10:00

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
#>  1 2026-05-10 04:00:00       21.8         76.2         0         0     
#>  2 2026-05-10 05:00:00       21.0         81.0         0         0     
#>  3 2026-05-10 06:00:00       20.8         83.4         0         0     
#>  4 2026-05-10 07:00:00       20.9         82.6         0         0.0009
#>  5 2026-05-10 08:00:00       20.6         82.9         0         0     
#>  6 2026-05-10 09:00:00       19.9         84.0         0         0.548 
#>  7 2026-05-10 10:00:00       20.2         82.1         0         0.289 
#>  8 2026-05-10 11:00:00       19.5         84.9         0         0.409 
#>  9 2026-05-10 12:00:00       19.0         88.5         0         0.0009
#> 10 2026-05-10 13:00:00       21.0         81.0         0         0.472 
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
