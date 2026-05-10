# Get elevation from the USGS Elevation Point Query Service

Queries the USGS Elevation Point Query Service for one or more latitude
and longitude pairs. Coordinates are always interpreted as WGS84 (WKID
4326).

## Usage

``` r
get_usgs_elev(latitude, longitude, units = c("Meters", "Feet"))
```

## Arguments

- latitude, longitude:

  Numeric vectors of latitude and longitude in decimal degrees. Latitude
  values must be between -90 and 90; longitude values must be between
  -180 and 180. Western longitudes are negative.

- units:

  Unit for the returned elevation. Must be either `"Meters"` or
  `"Feet"`. Defaults to `"Meters"`.

## Value

A numeric vector of elevations in the requested units, with one element
for each input coordinate pair.

## Examples

``` r
if (FALSE) { # \dontrun{
get_usgs_elev(latitude = 39.102075, longitude = -96.594689)
} # }
```
