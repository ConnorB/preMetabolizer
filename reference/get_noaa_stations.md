# Get NOAA station information

Searches for NOAA weather stations in the NCEI daily-summaries (GHCND)
dataset using the NCEI Search Service API. This is the preferred way to
find station identifiers before downloading data with
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
or
[`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md).

## Usage

``` r
get_noaa_stations(
  bbox = NULL,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  text = NULL,
  limit = 1000L,
  offset = 0L
)
```

## Arguments

- bbox:

  Optional numeric vector `c(north, west, south, east)` in decimal
  degrees. When supplied only stations within this bounding box are
  returned. Use
  [`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md)
  to build a bounding box from a centre point and radius. When `NULL` no
  geographic filter is applied.

- start_date, end_date:

  Optional date range filter. Accepts `Date` objects or `"YYYY-MM-DD"`
  strings. When supplied only stations whose period of record overlaps
  this range are returned.

- data_types:

  Optional character vector of GHCND data type codes (e.g.,
  `c("TMAX", "TMIN")`). When supplied only stations carrying all
  requested types are returned.

- text:

  Optional character string. Filters stations by name.

- limit:

  Integer. Maximum number of stations to return (1–1000, default 1000).

- offset:

  Integer. Zero-based pagination offset (default 0).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with columns `station_id`, `name`, `latitude`, `longitude`, `elevation`,
`start_date`, `end_date`, and `data_coverage`. Returns an empty tibble
when no stations match the query.

## Details

Station identifiers in `station_id` are bare GHCND IDs (e.g.,
`"USW00023183"`) stripped of their `"GHCND:"` prefix. Pass them directly
to
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
or
[`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md).

This function replaces the previous MSHR flat-file approach. Station
metadata is now retrieved on demand from the NCEI Search Service API
rather than downloaded as a large fixed-width archive.

For distance-based searches use
[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
which builds the bounding box from a latitude, longitude, and search
radius.

## See also

[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
[`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md),
[`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md),
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md),
[`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# All GHCND stations in a region
get_noaa_stations(bbox = c(40, -100, 38, -98))

# Stations with at least temperature data since 2000
get_noaa_stations(
  data_types = c("TMAX", "TMIN"),
  start_date = "2000-01-01"
)
} # }
```
