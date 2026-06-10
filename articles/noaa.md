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

> **Note:** All functions in this vignette contact the NCEI API. Code
> chunks will not run during package installation if NCEI is
> unreachable.

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
#>     north      west     south      east 
#>  40.00778 -97.77271  38.20598 -95.45072
```

``` r

ks_stations <- get_noaa_stations(bbox = bbox)

glimpse(ks_stations)
#> Rows: 634
#> Columns: 6
#> $ station_id   <chr> "USW00013996", "USW00013984", "USW00003919", "USW00003936…
#> $ station_name <chr> "TOPEKA ASOS, KS US", "CONCORDIA ASOS, KS US", "SALINA MU…
#> $ latitude     <dbl> 39.07246, 39.55127, 38.77996, 39.13456, 38.32906, 38.9414…
#> $ longitude    <dbl> -95.62602, -97.65077, -97.64444, -96.67894, -96.19453, -9…
#> $ start_date   <date> 1946-08-01, 1885-05-01, 1952-01-01, 1960-06-01, 1950-10-…
#> $ end_date     <date> 2026-06-07, 2026-06-07, 2026-06-06, 2026-06-07, 2026-06-…
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
#> # A tibble: 21 × 6
#>    station_id  station_name             latitude longitude start_date end_date  
#>    <chr>       <chr>                       <dbl>     <dbl> <date>     <date>    
#>  1 USC00140911 BLUE RAPIDS, KS US           39.7     -96.7 1905-01-01 2026-06-08
#>  2 USC00141559 CLAY CENTER, KS US           39.4     -97.1 1902-04-16 2026-06-08
#>  3 USC00141593 CLIFTON, KS US               39.6     -97.3 1931-04-01 2026-06-05
#>  4 USC00141761 CONCORDIA 2 SE, KS US        39.6     -97.6 2003-01-01 2026-06-06
#>  5 USC00141762 CONCORDIA 2 SSE, KS US       39.5     -97.7 2024-03-21 2026-06-08
#>  6 USC00141858 COTTONWOOD FALLS, KS US      38.4     -96.5 1902-12-01 2025-02-28
#>  7 USC00141867 COUNCIL GROVE LAKE, KS …     38.7     -96.5 1964-06-01 2026-06-01
#>  8 USC00142548 EMPORIA 3 NW, KS US          38.4     -96.2 1979-06-01 2026-06-08
#>  9 USW00003936 MANHATTAN ASOS, KS US        39.1     -96.7 1960-06-01 2026-06-07
#> 10 USC00145039 MARION RESERVOIR, KS US      38.4     -97.1 1966-01-01 2026-06-05
#> # ℹ 11 more rows
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
#> # A tibble: 5 × 5
#>   distance_km station_id  station_name              latitude longitude
#>         <dbl> <chr>       <chr>                        <dbl>     <dbl>
#> 1        6.58 USW00003936 MANHATTAN ASOS, KS US         39.1     -96.7
#> 2       15.7  USC00148259 TUTTLE CREEK LAKE, KS US      39.2     -96.6
#> 3       24.0  USC00148563 WAMEGO 4 W, KS US             39.2     -96.4
#> 4       35.9  USC00148802 WHITE CITY, KS US             38.8     -96.7
#> 5       48.7  USC00141867 COUNCIL GROVE LAKE, KS US     38.7     -96.5
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

![](noaa_files/figure-html/map-candidates-1.png)

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
#> [1] "USW00003936"
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
#> Rows: 365
#> Columns: 6
#> $ station_id   <chr> "USW00003936", "USW00003936", "USW00003936", "USW00003936…
#> $ station_name <chr> "MANHATTAN ASOS, KS US", "MANHATTAN ASOS, KS US", "MANHAT…
#> $ date         <date> 2024-10-01, 2024-10-02, 2024-10-03, 2024-10-04, 2024-10-…
#> $ prcp         <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.…
#> $ tmax         <dbl> 23.3, 29.4, 36.7, 28.9, 37.2, 26.7, 25.6, 27.2, 31.1, 30.…
#> $ tmin         <dbl> 5.6, 2.8, 11.7, 13.9, 11.1, 6.7, 2.8, 8.3, 6.7, 12.2, 11.…
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

![](noaa_files/figure-html/plot-temp-1.png)

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
#> [1] "USW00003936"
```

See `vignette("ghcnh", package = "preMetabolizer")` for a full
hourly-data workflow.
