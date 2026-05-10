# Calculate dissolved oxygen saturation

Calculates dissolved oxygen saturation in mg/L from water temperature,
barometric pressure, and salinity. This is the `DO.sat` quantity
commonly required by stream metabolism models.

## Usage

``` r
calc_O2sat(temp_water, atmo_press, units = "atm", salinity = 0)
```

## Arguments

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- atmo_press:

  Numeric vector. Barometric pressure at the site.

- units:

  Character string. Units of `atmo_press`. Accepted values are `"atm"`,
  `"hPa"`, `"mbar"`, and `"kPa"`. Defaults to `"atm"`.

- salinity:

  Numeric vector. Salinity in parts per thousand. Defaults to freshwater
  (`0`).

## Value

Numeric vector of dissolved oxygen saturation in mg/L.

## Details

The calculation uses the Benson and Krause umol/kg fit reported by
Garcia and Gordon (1992), applies a vapor-pressure correction for
non-standard barometric pressure, and converts from mass-based
concentration to mg/L with a salinity-aware density correction.

## Examples

``` r
calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
#> [1] 10.08428

calc_O2sat(
  temp_water = c(5, 15, 25),
  atmo_press = c(101.2, 100.8, 100.5),
  units = "kPa"
)
#> [1] 12.754793 10.031146  8.193914
```
