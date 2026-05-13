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
#> $ air_temp              <dbl> 24.01, 22.73, 23.85, 25.35, 23.14, 23.23, 24.89,…
#> $ air_temp2_m           <dbl> 24.01, 22.73, 23.85, 25.35, 23.14, 23.23, 24.89,…
#> $ humidity              <dbl> 59.04, 67.23, 62.92, 61.56, 60.47, 66.20, 50.93,…
#> $ precip                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, NA, 0, 0…
#> $ precip24_hr           <dbl> 0.000, 0.000, 0.254, 0.000, 0.000, 0.000, 0.000,…
#> $ precip48_hr           <dbl> 0.000, 0.000, 0.254, 0.000, 0.000, 3.048, 0.000,…
#> $ precip72_hr           <dbl> 2.540, 3.048, 9.906, 8.382, 8.636, 13.970, 0.254…
#> $ wind_speed            <dbl> 1.0343, 2.8890, 2.3104, 2.0742, 4.0132, 2.3449, …
#> $ wind_speed2_m         <dbl> 1.0343, 2.8890, 2.3104, 2.0742, 2.4709, 2.3449, …
#> $ wind_direction        <dbl> 253, 191, 221, 250, 226, 165, 199, 212, 269, 160…
#> $ wind_direction2_m     <dbl> 253, 191, 221, 250, 260, 165, 157, 260, 269, 160…
#> $ wind_gust             <dbl> 4.8071, 5.3397, 3.4756, 4.2745, 5.7199, 4.5408, …
#> $ wind_gust2_m          <dbl> 4.8071, 5.3397, 3.4756, 4.2745, 4.0082, 4.5408, …
#> $ battery_voltage       <dbl> 14.05, 14.25, 14.09, 14.20, 14.15, 13.00, 13.75,…
#> $ soil_moisture         <dbl> 0.244, 0.367, 0.367, 0.489, 0.284, 0.480, 0.146,…
#> $ soil_temperature      <dbl> 24.62, 20.69, 22.97, 21.97, 22.96, 21.92, 25.53,…
#> $ soil_moisture5_cm     <dbl> 0.190, 0.236, 0.211, 0.389, 0.116, 0.248, NA, 0.…
#> $ soil_temperature5_cm  <dbl> 22.79, 19.88, 21.12, 29.10, 22.17, 21.14, NA, 22…
#> $ soil_moisture10_cm    <dbl> 0.201, 0.166, 0.314, 0.453, 0.267, 0.332, 0.135,…
#> $ soil_temperature10_cm <dbl> 23.35, 19.91, 21.37, 21.99, 22.02, 21.41, 24.25,…
#> $ soil_moisture20_cm    <dbl> 0.244, 0.367, 0.367, 0.489, 0.284, 0.480, 0.146,…
#> $ soil_temperature20_cm <dbl> 24.62, 20.69, 22.97, 21.97, 22.96, 21.92, 25.53,…
#> $ soil_moisture50_cm    <dbl> 0.237, NA, 0.377, 0.530, NA, NA, NA, 0.257, NA, …
#> $ soil_temperature50_cm <dbl> 23.54, NA, 22.47, NA, NA, NA, NA, 22.77, NA, 24.…
#> $ data_interval_minutes <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
#> $ recorded_time         <dttm> 2026-05-13 14:50:00, 2026-05-13 14:50:00, 2026-…
#> $ air_temp9_m           <dbl> NA, NA, NA, NA, 22.15, NA, 24.13, 24.28, NA, NA,…
#> $ wind_speed10_m        <dbl> NA, NA, NA, NA, 4.0132, NA, 5.5083, 0.1971, NA, …
#> $ wind_direction10_m    <dbl> NA, NA, NA, NA, 226, NA, 199, 212, NA, NA, 127, …
#> $ wind_gust10_m         <dbl> NA, NA, NA, NA, 5.7199, NA, 7.2749, 1.6660, NA, …
#> $ air_pressure          <dbl> NA, NA, NA, NA, 945.0479, NA, 953.1674, 1014.275…
#> $ solar_radiation       <dbl> NA, NA, NA, NA, 594.3578, NA, 581.4288, 624.1393…
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
#> 1          2 Altwein Rd   2026-05-13 14:50:00     24.0     59.0      0
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
#> $ air_temp           <dbl> 23.73, 23.35, 22.83, 22.02, 21.38, 20.79, 20.09, 18…
#> $ air_temp9_m        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ humidity           <dbl> 60.97, 63.17, 65.83, 69.98, 71.71, 74.71, 77.27, 82…
#> $ precip             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ wind_speed         <dbl> 1.7338, 0.2461, 1.4292, 1.5708, 0.4540, 0.0000, 0.0…
#> $ wind_speed2_m      <dbl> 1.7338, 0.2461, 1.4292, 1.5708, 0.4540, 0.0000, 0.0…
#> $ wind_speed10_m     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_direction     <dbl> 243, 248, 237, 173, 162, 160, 227, 144, 150, 111, 1…
#> $ wind_direction2_m  <dbl> 243, 248, 237, 173, 162, 160, 227, 144, 150, 111, 1…
#> $ wind_direction10_m <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wind_gust2_m       <dbl> 3.4756, 2.6767, 4.0082, 3.7419, 2.6767, 0.0000, 0.0…
#> $ wind_gust10_m      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ air_pressure       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ solar_radiation    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ soil_moist5        <dbl> 0.190, 0.190, 0.190, 0.190, 0.190, 0.190, 0.190, 0.…
#> $ soil_moist10       <dbl> 0.201, 0.201, 0.202, 0.202, 0.202, 0.202, 0.202, 0.…
#> $ soil_moist20       <dbl> 0.244, 0.244, 0.244, 0.244, 0.245, 0.245, 0.245, 0.…
#> $ soil_moist50       <dbl> 0.237, 0.237, 0.237, 0.237, 0.237, 0.237, 0.237, 0.…
#> $ soil_temp5         <dbl> 22.74, 22.65, 22.61, 22.61, 22.63, 22.66, 22.72, 22…
#> $ soil_temp10        <dbl> 23.36, 23.38, 23.43, 23.46, 23.52, 23.57, 23.64, 23…
#> $ soil_temp20        <dbl> 24.65, 24.71, 24.73, 24.77, 24.79, 24.82, 24.87, 24…
#> $ soil_temp50        <dbl> 23.54, 23.54, 23.52, 23.53, 23.52, 23.52, 23.52, 23…
#> $ water_level        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_level2       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ water_temp2        <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ date_time          <dttm> 2026-05-13 14:45:00, 2026-05-13 14:30:00, 2026-05-…
```

The returned `date_time` column is parsed as UTC.

``` r

range(recent$date_time, na.rm = TRUE)
#> [1] "2026-05-12 15:00:00 UTC" "2026-05-13 14:45:00 UTC"
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
#> 1    74 2026-05-13 14:25:00
#> 2    74 2026-05-13 14:30:00
#> 3    74 2026-05-13 14:35:00
#> 4    74 2026-05-13 14:40:00
#> 5    75 2026-05-13 14:45:00
#> 6    75 2026-05-13 14:50:00

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
#>  1 2026-05-12 15:00:00       22.7         59.2         0         0     
#>  2 2026-05-12 16:00:00       24.3         51.4         0         0     
#>  3 2026-05-12 17:00:00       25.6         47.7         0         0.367 
#>  4 2026-05-12 18:00:00       26.2         47.7         0         0.0790
#>  5 2026-05-12 19:00:00       27.7         39.7         0         0.162 
#>  6 2026-05-12 20:00:00       28.7         37.7         0         0.0309
#>  7 2026-05-12 21:00:00       29.6         34.3         0         0.786 
#>  8 2026-05-12 22:00:00       29.8         35.6         0         0.255 
#>  9 2026-05-12 23:00:00       29.2         36.5         0         0.0103
#> 10 2026-05-13 00:00:00       28.2         39.6         0         0     
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
