# Find NOAA stations near a location

Identifies NOAA weather stations within a specified radius of a target
location using geodesic distance calculations.

## Usage

``` r
closest_noaa_stations(
  latitude,
  longitude,
  dist_km,
  state = NULL,
  clean = TRUE,
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

  Numeric. Search radius in kilometers.

- state:

  Optional two-letter state code used to filter stations before
  calculating distances.

- clean:

  Logical. If `TRUE`, return cleaned station metadata from
  [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  and drop empty columns from the result.

- lat, long, lon:

  **\[deprecated\]** Use `latitude` and `longitude` instead.

## Value

A data frame of NOAA stations within `dist_km`, sorted by distance. The
first column is `distance_km`. Returns `NULL` when no stations are found
in the requested radius or when available station metadata does not
contain usable coordinates.

## Details

Distances are calculated with
[`geosphere::distGeo()`](https://rdrr.io/pkg/geosphere/man/distGeo.html)
using its default WGS84 ellipsoid.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find stations within 50 km of Konza Prairie Biological Station
closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50
)

# Find stations within 100 km of Lawrence, Kansas only
closest_noaa_stations(
  latitude = 38.9717,
  longitude = -95.2353,
  dist_km = 100,
  state = "KS"
)
} # }
```
