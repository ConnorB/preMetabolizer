# Downloading Kansas Mesonet data

## Introduction

The [Kansas Mesonet](https://mesonet.k-state.edu/) is a statewide
network of automated weather stations operated by Kansas State
University. It provides sub-hourly meteorological data — including air
temperature, relative humidity, precipitation, solar radiation, wind,
and soil measurements — for more than 60 stations across Kansas.

This vignette walks through the workflow for discovering stations,
browsing the variable catalog, downloading data, and reading cached
files using the preMetabolizer Mesonet functions. We use **Konza
Prairie**, a long-term ecological research (LTER) site in northeastern
Kansas, as our example station.

> **Note:** Functions that contact the Mesonet API require an internet
> connection. Set `PREMETABOLIZER_RUN_VIGNETTES=true` to run the API
> chunks; they are skipped by default during package installation and
> checks.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Caching downloaded data

Repeated calls to the Kansas Mesonet API download the same data on every
run. This vignette saves each result to a local cache directory the
first time it is downloaded and reloads from disk on subsequent runs.
The cache lives in
`tools::R_user_dir("preMetabolizer", which = "cache")`, a
platform-appropriate, user-specific directory that persists across
sessions. Each data-fetching chunk below checks for a cached `.rds`
file, downloads and saves on the first run, and reloads from disk on all
subsequent runs.

## Explore the variable catalog

[`ks_meso_vars()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_vars.md)
retrieves and parses the current Kansas Mesonet variable metadata,
including CSV headings, variable names, units, and descriptions.

``` r

cache_file <- file.path(cache_dir, "ks_meso_vars.rds")
if (!file.exists(cache_file)) {
  mesonet_vars <- ks_meso_vars()
  saveRDS(mesonet_vars, cache_file)
} else {
  mesonet_vars <- readRDS(cache_file)
}
```

Search the catalog to find variables of interest:

``` r

mesonet_vars |>
  filter(grepl("temp|precip|solar|humidity", desc, ignore.case = TRUE)) |>
  select(var, tidy_name, units, desc)
```

## Discover stations

[`ks_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_stations.md)
retrieves metadata for all Mesonet stations, including location, network
affiliation, and whether the station supports FW13 fire weather reports.

``` r

cache_file <- file.path(cache_dir, "ks_meso_stations.rds")
if (!file.exists(cache_file)) {
  stations <- ks_meso_stations()
  saveRDS(stations, cache_file)
} else {
  stations <- readRDS(cache_file)
}

glimpse(stations)
```

Filter to find Konza Prairie and confirm the exact station name used by
the API:

``` r

konza <- stations |>
  filter(grepl("Konza", station_name, ignore.case = TRUE))

konza
```

The station name recognized by the API is `"Konza Prairie"`.

## Check station activity

[`ks_meso_station_activity()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_station_activity.md)
shows the observation intervals available at each station and the date
range of archived data. This is useful for verifying that a station has
data for your study period and identifying the finest available temporal
resolution.

``` r

cache_file <- file.path(cache_dir, "ks_meso_station_activity.rds")
if (!file.exists(cache_file)) {
  activity <- ks_meso_station_activity()
  saveRDS(activity, cache_file)
} else {
  activity <- readRDS(cache_file)
}

activity |>
  filter(station_name == "Konza Prairie")
```

## Check the most recent observation

[`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
returns the timestamp of the latest ingested observation for every
station at a given interval, which is helpful for monitoring live data
pipelines.

``` r

cache_file <- file.path(cache_dir, "ks_meso_most_recent_hour.rds")
if (!file.exists(cache_file)) {
  recent <- ks_meso_most_recent(interval = "hour")
  saveRDS(recent, cache_file)
} else {
  recent <- readRDS(cache_file)
}

recent |>
  filter(station_name == "Konza Prairie")
```

## Retrieve time-series data

[`ks_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_time_series.md)
retrieves Mesonet data for one or more stations and returns a tibble
directly. Large date ranges are split into chunks automatically to stay
within the API record limit.

Here we retrieve hourly data for Konza Prairie for the 2024 calendar
year, requesting air temperature, relative humidity, precipitation, and
surface pressure.

``` r

cache_file <- file.path(cache_dir, "ks_meso_konza_hourly_2024.rds")
if (!file.exists(cache_file)) {
  konza_hourly <- ks_meso_time_series(
    stations   = "Konza Prairie",
    start_date = "2024-01-01",
    end_date   = "2024-12-31",
    interval   = "hour",
    vars       = c("TEMP2MAVG", "RELHUM2MAVG", "PRECIP", "PRESSUREAVG")
  )
  saveRDS(konza_hourly, cache_file)
} else {
  konza_hourly <- readRDS(cache_file)
}

glimpse(konza_hourly)
```

## Example workflow: monthly precipitation

With the data in hand, standard `dplyr` and `ggplot2` workflows apply.
The example below summarizes monthly precipitation totals and daily
temperature ranges.

``` r

monthly_precip <- konza_hourly |>
  mutate(month = lubridate::floor_date(timestamp, "month")) |>
  group_by(month) |>
  summarise(
    precip_mm    = sum(precip, na.rm = TRUE),
    temp_mean_C  = mean(temp2_mavg, na.rm = TRUE),
    .groups      = "drop"
  )

ggplot(monthly_precip, aes(month, precip_mm)) +
  geom_col(fill = "#4575b4") +
  labs(
    x = NULL,
    y = "Precipitation (mm)",
    title = "Monthly precipitation — Konza Prairie 2024"
  ) +
  theme_bw()
```

## FW13 fire weather data

[`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
retrieves fire weather records in the standard FW13 format used by USDA
Forest Service fire behavior modeling tools.

``` r

cache_file <- file.path(cache_dir, "ks_meso_fw13_konza_2024-04.rds")
if (!file.exists(cache_file)) {
  fw13_records <- ks_meso_fw13(
    station    = "Konza Prairie",
    start_date = "2024-04-01",
    end_date   = "2024-04-30"
  )
  saveRDS(fw13_records, cache_file)
} else {
  fw13_records <- readRDS(cache_file)
}

head(fw13_records, 3)
```
