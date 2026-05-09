# Convert Barometric Pressure to Atmospheres

This helper function converts barometric pressure from various units to
atmospheres (\[atm\]). It is intended for internal use within the
package.

## Usage

``` r
convert_pressure_to_atm(pressure, units)
```

## Arguments

- pressure:

  Numeric. Barometric pressure value(s) to be converted.

- units:

  Character. Units of the input barometric pressure. Accepted values are
  `"atm"`, `"hPa"`, `"mbar"`, `"kPa"`, `"Torr"`, `"psi"`, and `"bar"`.

## Value

Numeric. Barometric pressure in atmospheres (\[atm\]).
