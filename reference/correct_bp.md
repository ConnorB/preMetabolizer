# Correct Barometric Pressure for Elevation Change

Adjusts barometric pressure from one elevation to another using the
barometric formula. Always returns a plain numeric vector.

## Usage

``` r
correct_bp(
  station_bp,
  air_temp,
  station_elev,
  site_elev,
  from_units = "kPa",
  to_units = "kPa"
)
```

## Arguments

- station_bp:

  Numeric. Barometric pressure at the station.

- air_temp:

  Numeric. Air temperature at the station in degrees Celsius.

- station_elev:

  Numeric. Elevation of the station in meters.

- site_elev:

  Numeric. Elevation of the target site in meters.

- from_units:

  Character. Units of `station_bp`. Defaults to `"kPa"`. See
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  for accepted values.

- to_units:

  Character. Desired output units. Defaults to `"kPa"`. See
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  for accepted values.

## Value

Numeric vector of corrected barometric pressure in `to_units`.

## See also

[`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
for supported pressure units.

## Examples

``` r
correct_bp(
  station_bp = c(101.3, 100.5), air_temp = c(15, 10),
  station_elev = 300, site_elev = 500
)
#> [1] 98.92626 98.10392
```
