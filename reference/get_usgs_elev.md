# Get elevation from the USGS Elevation Point Query Service

Queries the USGS Elevation Point Query Service for one or more latitude
and longitude pairs. Coordinates are interpreted as WGS84 (WKID 4326).
Requests are performed in parallel for improved performance when
querying multiple points. Duplicated coordinate pairs are only queried
once, and successful results are cached for the rest of the R session,
so repeated calls with the same coordinates do not re-contact the
service.

## Usage

``` r
get_usgs_elev(
  latitude,
  longitude,
  units = c("meters", "feet", "m", "ft"),
  details = FALSE
)
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

- details:

  Logical; if `FALSE` (the default), return a numeric elevation vector.
  If `TRUE`, return a tibble that also reports the USGS raster ID,
  raster resolution, source date when supplied by the service, and a
  status for each coordinate pair.

## Value

A numeric vector of elevations in the requested units, with one element
for each input coordinate pair. If an elevation cannot be retrieved for
a point, `NA_real_` is returned for that location and one warning is
issued for the failed points. With `details = TRUE`, a tibble is
returned with the elevation and response metadata for each coordinate
pair.

## Details

Elevations are interpolated from USGS 3DEP digital elevation models.
They are not surveyed elevations; accuracy varies with the source data
at each location. Use `details = TRUE` to retain the available raster
metadata when elevations need to be audited or compared.

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
