# Finding NOAA stations

## Introduction

NOAA station metadata are often the first thing you need when preparing
meteorological inputs for stream metabolism models. The station table
tells you which stations exist near a study site, which station
identifiers are available, and whether the station has coordinates that
can be used for distance searches.

preMetabolizer provides two helpers for this workflow:

- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  downloads and cleans station metadata from NOAA’s Meteorological
  Station Historical Repository.
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  finds stations within a distance of a target latitude and longitude.

> **Note:** These functions contact NOAA the first time they need
> station metadata. The downloaded file is cached locally and reused on
> later calls. Code chunks marked with `#| eval: false` are intended to
> be run interactively.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Download station metadata

Use
[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
to retrieve the cleaned NOAA station table. The cleaned table removes
records without usable coordinates, filters out balloon platforms, and
groups records by `GHCND_ID`.

``` r

stations <- get_noaa_stations(debug = FALSE)

glimpse(stations)
```

If you only need stations from one U.S. state, use the `state` argument.
State codes are case-insensitive.

``` r

ks_stations <- get_noaa_stations(state = "KS", debug = FALSE)

ks_stations |>
  select(GHCND_ID, NAME_PRINCIPAL, LAT_DEC, LON_DEC, ELEV_GROUND) |>
  arrange(NAME_PRINCIPAL)
```

## Find nearby stations

For site-level work, it is usually easier to start from a latitude and
longitude.
[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
calculates geodesic distance from a target coordinate to each station
and returns stations within a search radius.

This example searches within 50 km of Konza Prairie Biological Station.

``` r

konza_noaa <- closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50,
  state = "KS"
)

konza_noaa |>
  select(distance_km, GHCND_ID, NAME_PRINCIPAL, LAT_DEC, LON_DEC) |>
  arrange(distance_km)
```

The `state` argument is optional, but it can make searches faster and
easier to inspect when you know the site is far from a state border.

## Compare search radii

A useful pattern is to try a narrow search radius first, then widen it
if there are no stations with the variables or time span you need.

``` r

near_25 <- closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 25,
  state = "KS"
)

near_100 <- closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 100,
  state = "KS"
)

list(
  within_25_km = nrow(near_25),
  within_100_km = nrow(near_100)
)
```

You can also map the candidates before choosing a station for download.

``` r

ggplot(konza_noaa, aes(LON_DEC, LAT_DEC)) +
  geom_point(aes(size = distance_km), color = "#2c7fb8") +
  geom_point(
    aes(x = -96.6117151, y = 39.1068806),
    inherit.aes = FALSE,
    color = "#d95f0e",
    size = 3
  ) +
  coord_quickmap() +
  labs(
    x = "Longitude",
    y = "Latitude",
    size = "Distance (km)",
    title = "NOAA stations near Konza Prairie"
  ) +
  theme_bw()
```

## Use station IDs with GHCNh data

The `GHCND_ID` column identifies stations in NOAA’s Global Historical
Climatology Network. For hourly GHCNh downloads, pass one station ID to
[`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md).

``` r

station_id <- konza_noaa |>
  arrange(distance_km) |>
  pull(GHCND_ID) |>
  first()

station_id
```

Once you have a station ID, see
[`vignette("ghcnh", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/ghcnh.md)
for downloading and reading hourly observations.
