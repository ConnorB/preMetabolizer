# Convert UTC time to solar time

Converts a UTC datetime to mean or apparent solar time for a given
longitude. Uses a high-precision offset for longitude and applies the
equation of time for apparent solar time.

## Usage

``` r
convert_UTC_to_solartime(
  date.time,
  longitude,
  time.type = c("mean solar", "apparent solar")
)
```

## Arguments

- date.time:

  POSIXct vector in UTC.

- longitude:

  Numeric longitude in decimal degrees. Western longitudes are negative.

- time.type:

  One of `"mean solar"` (default) or `"apparent solar"`.

## Value

A POSIXct datetime in mean or apparent solar time (still tz = "UTC").

## Details

Apparent solar time, or true solar time, is noon when the sun is at its
zenith. Mean solar time approximates apparent solar time but keeps noons
exactly 24 hours apart. Elsewhere in this package, variables named
`solar.time` are mean solar time.

## Examples

``` r
utc <- as.POSIXct("2025-04-30 12:00:00", tz = "UTC")
convert_UTC_to_solartime(utc, -90, time.type = "mean solar")
#> [1] "2025-04-30 06:00:59 UTC"
convert_UTC_to_solartime(utc, -90, time.type = "apparent solar")
#> [1] "2025-04-30 06:03:38 UTC"
```
