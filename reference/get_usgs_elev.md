# Get elevation from the USGS Elevation Point Query Service

Queries the USGS Elevation Point Query Service for one or more latitude
and longitude pairs. Coordinates are interpreted as WGS84 (WKID 4326).
Requests are performed in parallel for improved performance when
querying multiple points.

## Usage

``` r
get_usgs_elev(latitude, longitude, units = c("meters", "feet", "m", "ft"))
```

## Arguments

- latitude, longitude:

  Numeric vectors of latitude and longitude in decimal degrees. Latitude
  values must be between -90 and 90; longitude values must be between
  -180 and 180. Western longitudes are negative. Inputs must be the same
  length.

- units:

  Character scalar specifying the output elevation units. Accepted
  values are `"meters"`, `"m"`, `"feet"`, and `"ft"`. Matching is case
  insensitive. Defaults to `"meters"`.

## Value

A numeric vector of elevations in the requested units, with one element
for each input coordinate pair. If an elevation cannot be retrieved for
a point, `NA_real_` is returned for that location and a warning is
issued.

## Examples

``` r
if (FALSE) { # \dontrun{
get_usgs_elev(
  latitude = 39.102075,
  longitude = -96.594689
)

get_usgs_elev(
  latitude = c(39.102075, 38.8977),
  longitude = c(-96.594689, -77.0365),
  units = "ft"
)
} # }
```
