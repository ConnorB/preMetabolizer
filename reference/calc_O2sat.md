# Calculate dissolved oxygen saturation

Calculates the dissolved oxygen concentration in equilibrium with the
atmosphere from water temperature, barometric pressure, and salinity.
This is the `DO.sat` quantity commonly required by stream metabolism
models.

## Usage

``` r
calc_O2sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  out_units = "mg/L"
)
```

## Arguments

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- atmo_press:

  Numeric vector. Barometric pressure at the site.

- units:

  Character string. Units of `atmo_press`. See
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  for accepted pressure units. Defaults to `"atm"`.

- salinity:

  Numeric vector. Salinity in parts per thousand. Defaults to freshwater
  (`0`).

- out_units:

  Character string. Output concentration units, either `"mg/L"` (the
  default) or `"umol/L"`.

## Value

Numeric vector of dissolved oxygen saturation in the requested units.

## Details

The calculation uses the Benson and Krause umol/kg fit reported by
Garcia and Gordon (1992), applies a vapor-pressure correction for
non-standard barometric pressure, and converts from mass-based
concentration to a volume basis with a salinity-aware density
correction.

## References

Garcia, H.E., and Gordon, L.I. (1992). Oxygen solubility in seawater:
better fitting equations. Limnology and Oceanography, 37(6), 1307-1312.

## See also

[`calc_CO2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2sat.md),
[`calc_CH4sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CH4sat.md),
[`calc_N2Osat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2Osat.md)

## Examples

``` r
calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
#> [1] 10.0839

calc_O2sat(
  temp_water = c(5, 15, 25),
  atmo_press = c(101.2, 100.8, 100.5),
  units = "kPa"
)
#> [1] 12.754311 10.030760  8.193614

# Return micromoles per liter instead of mg/L
calc_O2sat(temp_water = 15, atmo_press = 1, out_units = "umol/L")
#> [1] 315.1338
```
