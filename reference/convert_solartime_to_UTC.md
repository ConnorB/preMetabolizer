# Convert DateTime from Local Solar Time to UTC

Converts a datetime from local solar time back to UTC. Input time may be
either apparent solar time (sun at zenith at noon) or mean solar time
(noons exactly 24 hours apart).

## Usage

``` r
convert_solartime_to_UTC(
  any.solar.time,
  longitude,
  time.type = c("apparent solar", "mean solar")
)
```

## Arguments

- any.solar.time:

  A POSIXct datetime object in solar time (mean or apparent). Must have
  tz = "UTC".

- longitude:

  Numeric longitude in decimal degrees (east positive, west negative).

- time.type:

  Character string: either `"mean solar"` or `"apparent solar"`.
  "apparent solar", i.e. true solar time, is noon when the sun is at its
  zenith. "mean solar" approximates apparent solar time but with noons
  exactly 24 hours apart.

## Value

A POSIXct datetime in UTC.

## References

Yard, Bennett, Mietz, Coggins, Stevens, Hueftle, and Blinn. 2005.
Influence of topographic complexity on solar insolation estimates for
the Colorado River, Grand Canyon, AZ. Ecological Modelling.
