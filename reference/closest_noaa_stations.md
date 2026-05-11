# Find NOAA stations near a location

Identifies NOAA GHCND weather stations within a specified radius of a
target location. Station candidates are retrieved via the NCEI Search
API using a bounding box and then filtered to the exact circular radius
using geodesic distance.

## Usage

``` r
closest_noaa_stations(
  latitude,
  longitude,
  dist_km,
  start_date = NULL,
  end_date = NULL,
  data_types = NULL,
  lat = lifecycle::deprecated(),
  long = lifecycle::deprecated(),
  lon = lifecycle::deprecated()
)
```

## Arguments

- latitude, longitude:

  Numeric. Target coordinates in decimal degrees. Latitude must be
  between -90 and 90; longitude must be between -180 and 180. Western
  longitudes are negative.

- dist_km:

  Numeric. Search radius in kilometres.

- start_date, end_date:

  Optional date range. When supplied only stations whose period of
  record overlaps this range are returned. Accepts `Date` objects or
  `"YYYY-MM-DD"` strings.

- data_types:

  Optional character vector of GHCND data type codes (e.g.,
  `c("TMAX", "TMIN")`). When supplied only stations carrying all
  requested types are returned.

- lat, long, lon:

  **\[deprecated\]** Use `latitude` and `longitude` instead.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of NOAA stations within `dist_km`, sorted by ascending distance. The
first column is `distance_km`; remaining columns are those returned by
[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md).
Returns `NULL` when no stations are found within the requested radius.

## Details

Distances are calculated with
[`geosphere::distGeo()`](https://rdrr.io/pkg/geosphere/man/distGeo.html)
using the default WGS84 ellipsoid.

The search first queries the NCEI Search API using a square bounding box
of side `2 * dist_km` centred on the target point, then trims the result
to the circular radius. This requires one API call and avoids
downloading the entire station database.

## See also

[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md),
[`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md),
[`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Stations within 50 km of Konza Prairie Biological Station
closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50
)

# Restrict to stations with daily temperature data since 2000
closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 100,
  data_types = c("TMAX", "TMIN"),
  start_date = "2000-01-01"
)
} # }
```
