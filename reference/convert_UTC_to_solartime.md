# Convert UTC Time to Mean or Apparent Solar Time

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

  A POSIXct datetime object in UTC timezone.

- longitude:

  Longitude in decimal degrees (east positive, west negative).

- time.type:

  One of `"mean solar"` (default) or `"apparent solar"`.

## Value

A POSIXct datetime in mean or apparent solar time (still tz = "UTC").

## Details

"apparent solar", i.e. true solar time, is noon when the sun is at its
zenith. "mean solar" approximates apparent solar time but with noons
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
