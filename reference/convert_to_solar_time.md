# Convert a datetime to local solar time

Converts a datetime to mean or apparent (true) solar time at a given
longitude. Mean solar time uses a constant 15 deg/hour longitude offset.
Apparent solar time additionally applies the equation of time and is
computed via
[`SunCalcMeeus::solar_time()`](https://rdrr.io/pkg/SunCalcMeeus/man/solar_time.html).

## Usage

``` r
convert_to_solar_time(dateTime, longitude, type = c("mean", "apparent"))

convert_from_solar_time(
  solar_datetime,
  longitude,
  type = c("mean", "apparent")
)
```

## Arguments

- dateTime:

  A datetime vector. POSIXct in any time zone is accepted; the function
  operates on the underlying instant, so a CDT timestamp and the
  matching UTC timestamp produce identical results. Character or `Date`
  input is coerced with `as.POSIXct(., tz = "UTC")`.

- longitude:

  Numeric. Site longitude in decimal degrees; western longitudes are
  negative.

- type:

  One of `"mean"` (default) or `"apparent"`.

- solar_datetime:

  A POSIXct vector of solar-time values. The clock reading is
  interpreted as solar time even though the tzone attribute is UTC; this
  matches the convention used by `streamMetabolizer` and `SunCalcMeeus`.

## Value

A POSIXct vector with class `c("solar_date", "POSIXct", "POSIXt")`
(`convert_to_solar_time()`) or `c("POSIXct", "POSIXt")`
(`convert_from_solar_time()`). The tzone attribute is `"UTC"` in both
cases; the clock reading carries the meaning.

## Details

These helpers are named `convert_to_solar_time()` /
`convert_from_solar_time()` to avoid shadowing the original
`streamMetabolizer::convert_UTC_to_solartime()` and
`streamMetabolizer::convert_solartime_to_UTC()`, which can still be
called directly from `streamMetabolizer` if you need their behaviour.

The mean offset is `longitude / 15 * 3600` seconds. The apparent path
delegates to
[`SunCalcMeeus::solar_time()`](https://rdrr.io/pkg/SunCalcMeeus/man/solar_time.html),
which applies the equation of time using Meeus's algorithms.

`convert_from_solar_time()` inverts the forward conversion by computing
the forward offset *at the input solar instant* and subtracting it. This
is exact for `type = "mean"`. For `type = "apparent"` it is accurate to
within roughly the daily change in the equation of time (~30 s/day),
which is well below typical sensor sampling intervals.

## See also

[`SunCalcMeeus::solar_time()`](https://rdrr.io/pkg/SunCalcMeeus/man/solar_time.html),
`streamMetabolizer::convert_UTC_to_solartime()`,
`streamMetabolizer::convert_solartime_to_UTC()`.

## Examples

``` r
utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")

convert_to_solar_time(utc, longitude = -96.6)
#> [1] "2024-06-21 11:33:36 solar"
convert_to_solar_time(utc, longitude = -96.6, type = "apparent")
#> [1] "2024-06-21 11:31:37 solar"

# A non-UTC POSIXct gives the same result, since the underlying instant
# is what matters.
local <- as.POSIXct("2024-06-21 13:00:00", tz = "America/Chicago")
convert_to_solar_time(local, longitude = -96.6)
#> [1] "2024-06-21 11:33:36 solar"

solar <- convert_to_solar_time(utc, longitude = -96.6)
convert_from_solar_time(solar, longitude = -96.6)
#> [1] "2024-06-21 18:00:00 UTC"
```
