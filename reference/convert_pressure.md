# Convert Barometric Pressure Between Units

Converts barometric pressure from one unit to another using exact
conversion factors. Always returns a plain numeric vector.

## Usage

``` r
convert_pressure(pressure, from, to = "atm")
```

## Arguments

- pressure:

  Numeric. Barometric pressure value(s) to be converted.

- from:

  Character. Units of the input barometric pressure. Accepted values are
  `"atm"`, `"hPa"`, `"mbar"`, `"kPa"`, `"Pa"`, `"Torr"`, `"psi"`, and
  `"bar"`.

- to:

  Character. Target unit. Same accepted values as `from`. Defaults to
  `"atm"`.

## Value

Numeric vector of barometric pressure in the requested unit.

## Examples

``` r
convert_pressure(101.3, from = "kPa", to = "atm")
#> [1] 0.9997533
convert_pressure(1013.25, from = "hPa", to = "Pa")
#> [1] 101325
```
