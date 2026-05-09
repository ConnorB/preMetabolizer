# Find NOAA Stations Within Specified Radius

Identifies NOAA weather stations within a specified radius of a given
location using geodesic distance calculations. Returns station metadata
including distance from the target location.

## Usage

``` r
closest_noaa_stations(lat, long, dist_km, state = NULL, clean = TRUE)
```

## Arguments

- lat:

  Numeric latitude of the target location in decimal degrees

- long:

  Numeric longitude of the target location in decimal degrees

- dist_km:

  Numeric search radius in kilometers

- state:

  Optional two-letter state code to filter stations

- clean:

  Logical indicating whether to return cleaned data (default: TRUE)

## Value

A data frame of NOAA stations within the specified radius, including:

- Station identifiers (GHCND_ID, WBAN_ID, etc.)

- Station names and locations

- Elevation data

- Operational dates

- Distance from target location (in km)

Returns NULL if no stations are found within the radius.

## Details

Distance calculations use
[`geosphere::distGeo()`](https://rdrr.io/pkg/geosphere/man/distGeo.html)
for highly accurate geodesic distance calculations. See the function
documentation for details about the ellipsoid model and algorithm used.

## Examples
