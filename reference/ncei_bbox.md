# Compute a bounding box around a point

Returns a bounding box vector suitable for the `bbox` argument of
[`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
and
[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md).

## Usage

``` r
ncei_bbox(latitude, longitude, dist_km)
```

## Arguments

- latitude, longitude:

  Numeric. Target coordinates in decimal degrees. Latitude must be
  between -90 and 90; longitude between -180 and 180.

- dist_km:

  Positive numeric. Half-width of the bounding box in kilometres.

## Value

A numeric vector `c(north, west, south, east)`.

## Examples

``` r
# Bounding box 50 km around Konza Prairie
ncei_bbox(39.1, -96.6, 50)
#>     north      west     south      east 
#>  39.55045 -97.18044  38.64955 -96.01956 
```
