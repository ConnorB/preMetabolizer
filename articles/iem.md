# Working with Iowa Environmental Mesonet data

## Introduction

The [Iowa Environmental Mesonet](https://mesonet.agron.iastate.edu/)
(IEM) provides public access to many meteorological and hydrological
observing networks. IEM groups stations into network identifiers such as
`IA_ASOS`, `IA_COOP`, and many state or special-purpose networks.

preMetabolizer provides helpers for selected IEM API v1 endpoints:

- [`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md)
  lists available IEM network identifiers.
- [`iem_stations()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
  retrieves station metadata for one network.
- [`iem_station()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
  looks up one station identifier across networks.
- [`iem_current()`](https://connorb.github.io/preMetabolizer/reference/iem_current.md)
  retrieves current observations.
- [`iem_ob_history()`](https://connorb.github.io/preMetabolizer/reference/iem_ob_history.md)
  retrieves one local day of station observations.
- [`iem_daily()`](https://connorb.github.io/preMetabolizer/reference/iem_daily.md)
  retrieves daily summary observations.

> **Note:** These functions contact IEM web services and require an
> internet connection. Set `PREMETABOLIZER_RUN_VIGNETTES=true` to run
> the API chunks; they are skipped by default during package
> installation and checks.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Caching downloaded data

Repeated calls to the IEM API download the same data on every run. This
vignette saves each result to a local cache directory the first time it
is downloaded and reloads from disk on subsequent runs. The cache lives
in `tools::R_user_dir("preMetabolizer", which = "cache")`, a
platform-appropriate, user-specific directory that persists across
sessions. Each data-fetching chunk below checks for a cached `.rds`
file, downloads and saves on the first run, and reloads from disk on all
subsequent runs.

## Discover networks and stations

Start with
[`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md)
when you need to find the exact network identifier for a station source.
The `network` column is the value to pass to other IEM helpers.

``` r

cache_file <- file.path(cache_dir, "iem_networks.rds")
if (!file.exists(cache_file)) {
  networks <- iem_networks()
  saveRDS(networks, cache_file)
} else {
  networks <- readRDS(cache_file)
}

networks |>
  filter(grepl("Iowa", network_name)) |>
  select(network, network_name, tzname)
```

Station metadata for a network includes station identifiers, names,
coordinates, archive dates, and the station time zone when IEM provides
it.

``` r

cache_file <- file.path(cache_dir, "iem_stations_ia_asos.rds")
if (!file.exists(cache_file)) {
  ia_asos <- iem_stations("IA_ASOS")
  saveRDS(ia_asos, cache_file)
} else {
  ia_asos <- readRDS(cache_file)
}

ia_asos |>
  select(station_id, station_name, network, tzname, archive_begin, longitude, latitude) |>
  arrange(station_id)
```

Some station identifiers appear in more than one network. Use
[`iem_station()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
when you want to check how IEM resolves a station ID before requesting
data.

``` r

cache_file <- file.path(cache_dir, "iem_station_amw.rds")
if (!file.exists(cache_file)) {
  amw <- iem_station("AMW")
  saveRDS(amw, cache_file)
} else {
  amw <- readRDS(cache_file)
}

amw |>
  select(station_id, station_name, network, tzname, archive_begin)
```

## Retrieve current observations

[`iem_current()`](https://connorb.github.io/preMetabolizer/reference/iem_current.md)
requires at least one filter. You can request all current observations
in a network, stations within a network, stations by forecast office, or
one or more station identifiers.

``` r

cache_file <- file.path(cache_dir, "iem_current_ia_asos.rds")
if (!file.exists(cache_file)) {
  current <- iem_current(network = "IA_ASOS")
  saveRDS(current, cache_file)
} else {
  current <- readRDS(cache_file)
}

current |>
  select(station_id, station_name, utc_valid, tmpf, dwpf, sknt, drct, mslp) |>
  arrange(station_id)
```

For station-specific work, include both `network` and `stations` so
shared station identifiers do not surprise you.

``` r

cache_file <- file.path(cache_dir, "iem_current_dsm_amw.rds")
if (!file.exists(cache_file)) {
  dsm_current <- iem_current(
    network = "IA_ASOS",
    stations = c("DSM", "AMW"),
    minutes = 120
  )
  saveRDS(dsm_current, cache_file)
} else {
  dsm_current <- readRDS(cache_file)
}

dsm_current |>
  select(station_id, station_name, utc_valid, tmpf, relh, sknt, pres)
```

## Retrieve one day of observations

[`iem_ob_history()`](https://connorb.github.io/preMetabolizer/reference/iem_ob_history.md)
returns observations for a single station and local station date. UTC
timestamp columns are parsed as UTC `POSIXct` values. Local timestamp
columns are returned as character values because station time zones vary
across networks.

``` r

cache_file <- file.path(cache_dir, "iem_ob_history_dsm_2024-06-01.rds")
if (!file.exists(cache_file)) {
  obs <- iem_ob_history(
    station = "DSM",
    network = "IA_ASOS",
    date = "2024-06-01"
  )
  saveRDS(obs, cache_file)
} else {
  obs <- readRDS(cache_file)
}

obs |>
  select(utc_valid, local_valid, tmpf, dwpf, sknt, drct, p01_i)
```

Hourly or sub-hourly records can be plotted directly after selecting the
variables you need.

``` r

obs |>
  filter(!is.na(tmpf)) |>
  ggplot(aes(utc_valid, tmpf)) +
  geom_line() +
  labs(
    x = "UTC time",
    y = "Air temperature (deg F)"
  )
```

## Retrieve daily summaries

Use
[`iem_daily()`](https://connorb.github.io/preMetabolizer/reference/iem_daily.md)
for daily summary observations. Provide either one `date`, or provide
`year` and optional `month` for a longer request.

``` r

cache_file <- file.path(cache_dir, "iem_daily_dsm_2024-06.rds")
if (!file.exists(cache_file)) {
  dsm_daily <- iem_daily(
    network = "IA_ASOS",
    station = "DSM",
    year = 2024,
    month = 6
  )
  saveRDS(dsm_daily, cache_file)
} else {
  dsm_daily <- readRDS(cache_file)
}

dsm_daily |>
  select(station_id, date, max_tmpf, min_tmpf, precip) |>
  arrange(date)
```

For a one-day network request, omit `station` and supply `date`.

``` r

cache_file <- file.path(cache_dir, "iem_daily_ia_asos_2024-06-01.rds")
if (!file.exists(cache_file)) {
  ia_day <- iem_daily("IA_ASOS", date = "2024-06-01")
  saveRDS(ia_day, cache_file)
} else {
  ia_day <- readRDS(cache_file)
}

ia_day |>
  select(station_id, date, max_tmpf, min_tmpf, precip) |>
  arrange(desc(date))
```

## Practical notes

IEM combines many observing networks, and each network can have
different available variables, units, reporting intervals, and station
identifiers. A good workflow is to first inspect
[`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md)
and
[`iem_stations()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md),
then request observations from a specific `network` and `station` pair.
