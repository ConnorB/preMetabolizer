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
#> $ object_id             <int> 2, 3, 4, 5, 6, 8, 9, 10, 13, 14, 15, 16, 17, 18,…
#> $ station_id            <int> 2, 3, 4, 5, 6, 8, 9, 10, 13, 14, 15, 16, 17, 18,…
#> $ station_name          <chr> "Altwein Rd", "Headwaters Ranch", "Holt Oaks Ran…
#> $ display_id            <chr> "BCALT", "KEHEA", "BCHOR", "BCARN", "BNLCP", "VV…
#> $ latitude              <dbl> 30.14957, 30.08845, 30.01853, 30.11047, 29.80180…
#> $ longitude             <dbl> -98.54044, -98.69757, -98.45631, -98.30463, -99.…
#> $ elevation             <int> 1730, 1959, 1416, 1316, 2163, 1881, 166, 339, 48…
#> $ air_temp              <dbl> 18.34, 17.70, 17.62, 19.34, 19.40, 20.99, 20.58,…
#> $ air_temp2_m           <dbl> 18.34, 17.70, 17.62, 19.34, 19.40, 20.99, 20.58,…
#> $ humidity              <dbl> 80.80, 85.60, 86.80, 79.64, 67.05, 50.88, 84.90,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, NA, 0, 0, 0…
#> $ precip24_hr           <dbl> 1.524, 1.778, 5.080, 3.048, 0.000, 0.000, 1.270,…
#> $ precip48_hr           <dbl> 2.540, 3.048, 9.652, 8.382, 8.636, 0.254, 1.270,…
#> $ precip72_hr           <dbl> 3.048, 3.302, 10.668, 9.906, 8.890, 0.254, 1.524…
#> $ wind_speed            <dbl> 0.0000, 0.0000, 0.0000, 0.0824, 2.3462, 0.2881, …
#> $ wind_speed2_m         <dbl> 0.0000, 0.0000, 0.0000, 0.0824, 1.4890, 0.0000, …
#> $ wind_direction        <dbl> 60, 50, 347, 354, 74, 163, 312, 310, 352, 279, 1…
#> $ wind_direction2_m     <dbl> 60, 50, 347, 354, 44, 9, 339, 310, 352, 252, 36,…
#> $ wind_gust             <dbl> 0.0000, 0.0000, 0.0000, 0.7193, 3.9429, 1.1662, …
#> $ wind_gust2_m          <dbl> 0.0000, 0.0000, 0.0000, 0.7193, 2.6767, 0.0000, …
#> $ battery_voltage       <dbl> 12.69, 12.76, 12.71, 12.63, 12.71, 13.13, 12.59,…
#> $ soil_moisture         <dbl> 0.251, 0.376, 0.386, 0.492, 0.301, 0.157, 0.149,…
#> $ soil_temperature      <dbl> 25.80, 21.66, 23.38, 22.61, 24.28, 26.70, 23.43,…
#> $ soil_moisture5_cm     <dbl> 0.203, 0.246, 0.272, 0.403, 0.144, NA, 0.161, 0.…
#> $ soil_temperature5_cm  <dbl> 25.58, 21.42, 22.98, 31.51, 22.83, NA, 23.32, 24…
#> $ soil_moisture10_cm    <dbl> 0.213, 0.172, 0.338, 0.461, 0.284, 0.145, 0.157,…
#> $ soil_temperature10_cm <dbl> 26.46, 21.99, 24.65, 23.67, 24.75, 27.06, 23.38,…
#> $ soil_moisture20_cm    <dbl> 0.251, 0.376, 0.386, 0.492, 0.301, 0.157, 0.149,…
#> $ soil_temperature20_cm <dbl> 25.80, 21.66, 23.38, 22.61, 24.28, 26.70, 23.43,…
#> $ soil_moisture50_cm    <dbl> 0.239, NA, 0.381, 0.530, NA, NA, 0.264, NA, 0.36…
#> $ soil_temperature50_cm <dbl> 23.16, NA, 22.26, NA, NA, NA, 22.97, NA, 24.80, …
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-05-12 04:05:00, 2026-05-12 04:05:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 20.28, 21.18, 22.13, NA, NA, 19.…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 2.3462, 0.2881, 1.0287, NA, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 74, 163, 312, NA, NA, 279, 11, 5…
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 3.9429, 1.1662, 2.1103, NA, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 945.8464, 954.2123, 1013.9550, N…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 0, 0, 0, NA, NA, 0, 0, 0, 0, 0, …
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
#> 1          2 Altwein Rd   2026-05-12 04:05:00     18.3     80.8      0
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
#> $ air_temp           <dbl> 18.44, 18.79, 18.88, 19.12, 19.53, 19.63, 19.50, 19…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 80.50, 79.09, 78.22, 76.80, 74.68, 74.30, 75.05, 73…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0…
#> $ wind_speed2_m      <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 119, 159, 179, 219, 230, 190, 216, 249, 127, 197, 2…
#> $ wind_direction2_m  <dbl> 119, 159, 179, 219, 230, 190, 216, 249, 127, 197, 2…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.203, 0.203, 0.202, 0.203, 0.203, 0.204, 0.204, 0.…
#> $ soil_moist10       <dbl> 0.213, 0.213, 0.214, 0.214, 0.214, 0.214, 0.214, 0.…
#> $ soil_moist20       <dbl> 0.251, 0.251, 0.251, 0.251, 0.251, 0.251, 0.251, 0.…
#> $ soil_moist50       <dbl> 0.239, 0.239, 0.239, 0.239, 0.240, 0.240, 0.240, 0.…
#> $ soil_temp5         <dbl> 25.61, 25.76, 25.91, 26.07, 26.24, 26.43, 26.59, 26…
#> $ soil_temp10        <dbl> 26.50, 26.65, 26.73, 26.87, 26.97, 27.09, 27.20, 27…
#> $ soil_temp20        <dbl> 25.81, 25.78, 25.76, 25.79, 25.77, 25.75, 25.68, 25…
#> $ soil_temp50        <dbl> 23.16, 23.15, 23.16, 23.16, 23.13, 23.14, 23.17, 23…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-05-12 04:00:00, 2026-05-12 03:45:00, 2026-05-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-05-11 04:10:00 UTC" "2026-05-12 04:00:00 UTC"
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
#> 1    66 2026-05-12 03:40:00
#> 2    66 2026-05-12 03:45:00
#> 3    66 2026-05-12 03:50:00
#> 4    65 2026-05-12 03:55:00
#> 5    65 2026-05-12 04:00:00
#> 6    65 2026-05-12 04:05:00

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
#>  1 2026-05-11 04:00:00       17.4         95.7     0.762          0.381
#>  2 2026-05-11 05:00:00       16.6         97.3     0.508          1.91 
#>  3 2026-05-11 06:00:00       16.2         99.9     0              1.42 
#>  4 2026-05-11 07:00:00       16.1         99.0     0              0.334
#>  5 2026-05-11 08:00:00       16.1         99.9     0              0.367
#>  6 2026-05-11 09:00:00       16.2         99.9     0.254          1.18 
#>  7 2026-05-11 10:00:00       16.0         98.1     0              0.697
#>  8 2026-05-11 11:00:00       15.8         97.6     0              1.48 
#>  9 2026-05-11 12:00:00       15.7         96.0     0              1.87 
#> 10 2026-05-11 13:00:00       16.1         94.5     0              2.43 
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
