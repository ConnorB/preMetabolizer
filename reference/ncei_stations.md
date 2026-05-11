# Search for NCEI weather stations

Searches for stations in NOAA's National Centers for Environmental
Information (NCEI) using the Common Access Search Service API.

## Usage

``` r
ncei_stations(
  dataset,
  bbox = NULL,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  text = NULL,
  limit = 100L,
  offset = 0L
)
```

## Arguments

- dataset:

  Character string. The dataset to search within, such as
  `"daily-summaries"` (GHCND) or `"global-hourly"` (ISD). See
  [`ncei_datasets()`](https://connorb.github.io/preMetabolizer/reference/ncei_datasets.md)
  for more dataset identifiers.

- bbox:

  Optional numeric vector of length 4 specifying the geographic search
  area as `c(north, west, south, east)` in decimal degrees. North and
  south must be between -90 and 90; west and east between -180 and 180.
  When `NULL` no geographic filter is applied.

- start_date, end_date:

  Optional date range filter. Only stations with data overlapping this
  period are returned. Accepts `Date` objects or `"YYYY-MM-DD"` strings.

- data_types:

  Optional character vector of data type codes. Only stations that
  include all requested types are returned.

- text:

  Optional character string. Filters results to stations whose names
  contain this text.

- limit:

  Integer. Maximum number of stations to return per page (default 100,
  max 1000).

- offset:

  Integer. Zero-based pagination offset (default 0).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per station and the following columns:

- `station_id`:

  Station identifier (dataset prefix stripped, e.g., `"USW00023183"`
  rather than `"GHCND:USW00023183"`).

- `station_name`:

  Station name.

- `latitude`,`longitude`:

  Decimal-degree coordinates.

- `start_date`,`end_date`:

  Period of record as `Date` objects.

## Details

This function calls
`https://www.ncei.noaa.gov/access/services/search/v1/data`.

To find stations near a specific point, compute a bounding box with
[`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md)
and pass it to `bbox`. For site-level station searches prefer
[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
which computes the bounding box automatically and filters by geodesic
distance.

## See also

[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md),
[`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md),
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# All daily-summaries stations in a region
ncei_stations(
  dataset = "daily-summaries",
  bbox = c(40, -100, 38, -98)
)

# Hourly stations with temperature data and a long record
ncei_stations(
  dataset = "global-hourly",
  start_date = "1990-01-01",
  end_date = Sys.Date()
)
} # }
```
