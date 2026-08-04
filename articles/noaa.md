# Finding NOAA stations

## Introduction

Meteorological inputs—temperature, precipitation, and barometric
pressure—are essential for computing dissolved oxygen saturation and gas
exchange in stream metabolism models. This vignette shows how to find
NOAA daily-summary stations near Kings Creek at Konza Prairie Biological
Station, Kansas, and download the weather data needed to accompany the
built-in `kings_discharge` dataset (water year 2025: 2024-10-01 through
2025-09-30).

preMetabolizer provides three helpers for station discovery:

- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  searches for GHCND stations via the NCEI Search Service API.
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  finds stations within a distance of a target coordinate and ranks them
  by geodesic distance.
- [`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
  is the lower-level function underlying both helpers; it can search any
  NCEI dataset.

> **Note:** All functions in this vignette contact the NCEI API. Set
> `PREMETABOLIZER_RUN_VIGNETTES=true` to run the API chunks; they are
> skipped by default during package installation and checks.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Study site

Kings Creek drains the Konza Prairie Biological Station near Manhattan,
Kansas (39.1069°N, 96.6117°W). The USGS monitoring location
`USGS-06879650` records daily discharge, gage height, and water
temperature throughout water year 2025.

``` r

lat_kings  <- 39.1068806
lon_kings  <- -96.6117151
wy_start   <- "2024-10-01"
wy_end     <- "2025-09-30"
```

## Search for stations

Use
[`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md)
to build a bounding box from a center point and radius, then pass it to
[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md):

``` r

bbox <- ncei_bbox(latitude = lat_kings, longitude = lon_kings, dist_km = 100)
bbox
```

``` r

ks_stations <- get_noaa_stations(bbox = bbox)

glimpse(ks_stations)
```

Filter to stations that carry the variables you need and span at least
part of water year 2025:

``` r

wx_stations <- get_noaa_stations(
  bbox       = bbox,
  data_types = c("TMAX", "TMIN", "PRCP"),
  start_date = wy_start,
  end_date   = wy_end
)

wx_stations |>
  select(station_id, station_name, latitude, longitude, start_date, end_date) |>
  arrange(station_name)
```

## Find nearby stations

[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
builds the bounding box automatically and returns stations sorted by
geodesic distance from the target point:

``` r

konza_noaa <- closest_noaa_stations(
  latitude   = lat_kings,
  longitude  = lon_kings,
  dist_km    = 50,
  data_types = c("TMAX", "TMIN", "PRCP"),
  start_date = wy_start,
  end_date   = wy_end
)

konza_noaa |>
  select(distance_km, station_id, station_name, latitude, longitude)
```

Map the candidates before choosing a station:

``` r

ggplot(konza_noaa, aes(longitude, latitude)) +
  geom_point(aes(size = distance_km), color = "#2c7fb8") +
  annotate("point", x = lon_kings, y = lat_kings, color = "#d95f0e", size = 3) +
  coord_quickmap() +
  labs(
    x     = "Longitude",
    y     = "Latitude",
    size  = "Distance (km)",
    title = "NOAA stations near Kings Creek"
  ) +
  theme_bw()
```

## Download daily weather data

Pick the nearest station and download daily temperature and
precipitation for water year 2025 with
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md):

``` r

station_id <- konza_noaa |>
  arrange(distance_km) |>
  pull(station_id) |>
  first()

station_id
```

``` r

daily_wx <- ncei_data(
  dataset    = "daily-summaries",
  stations   = station_id,
  start_date = wy_start,
  end_date   = wy_end,
  data_types = c("TMAX", "TMIN", "PRCP")
)

glimpse(daily_wx)
```

With `units = "metric"` (the default),
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
returns `tmax` and `tmin` in °C and `prcp` in mm. The `date` column is
already a `Date`:

``` r

ggplot(daily_wx, aes(date)) +
  geom_ribbon(aes(ymin = tmin, ymax = tmax), alpha = 0.3, fill = "#2c7fb8") +
  geom_line(aes(y = (tmax + tmin) / 2), color = "#2c7fb8") +
  labs(
    x     = NULL,
    y     = "Air temperature (°C)",
    title = "Daily temperature range near Kings Creek, WY 2025"
  ) +
  theme_bw()
```

## Barometric pressure and PAR

GHCND covers temperature and precipitation well, but barometric pressure
records are sparse in this region. For pressure and photosynthetically
active radiation (PAR), use
[`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
to pull modeled values from NASA POWER, or
[`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)
to download observed hourly pressure from the GHCNh archive.

``` r

stream_data <- tibble(
  dateTime = seq(
    as.POSIXct(paste(wy_start, "00:00:00"), tz = "UTC"),
    as.POSIXct(paste(wy_end, "23:00:00"), tz = "UTC"),
    by = "1 hour"
  )
)

nasa_wx <- get_nasa_data(
  data      = stream_data,
  latitude  = lat_kings,
  longitude = lon_kings,
  elev_m    = 320
)

glimpse(nasa_wx)
```

The `PSC` column contains elevation-corrected barometric pressure (kPa),
`light.obs` contains PAR (µmol/m²/s), `T2M` air temperature (°C), and
`PRECTOTCORR` precipitation (mm/hr).

## Use station IDs with GHCNh data

The `station_id` values returned by
[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
also identify GHCNh files. Pass them directly to
[`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md):

``` r

station_id
```

See `vignette("ghcnh", package = "preMetabolizer")` for a full
hourly-data workflow.
