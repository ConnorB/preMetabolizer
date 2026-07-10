# Calculate dissolved argon saturation

Calculates the dissolved argon concentration in equilibrium with the
atmosphere from water temperature, barometric pressure, and salinity,
using the Hamme and Emerson (2004) solubility fit for argon in
air-equilibrated water.

## Usage

``` r
calc_ar_sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  out_units = "umol/L"
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

  Character string. Output concentration units, either `"umol/L"` (the
  default) or `"mg/L"`.

## Value

Numeric vector of dissolved Ar saturation in the requested units.

## Details

The equilibrium concentration follows the Hamme and Emerson (2004) eq 1
fit with their Table 4 umol/kg coefficients, valid for water in
equilibrium with water-vapor-saturated air at 1 atm total pressure. A
vapor-pressure correction scales the 1 atm result to the supplied
barometric pressure, and a salinity-aware density correction converts
from a mass to a volume basis.

## References

Hamme, R.C., and Emerson, S.R. (2004). The solubility of neon, nitrogen
and argon in distilled water and seawater. Deep-Sea Research Part I,
51(11), 1517-1528.

## See also

[`calc_n2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_n2_sat.md),
[`calc_o2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_o2_sat.md),
[`calc_co2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_sat.md),
[`calc_ch4_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_ch4_sat.md),
[`calc_n2o_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_n2o_sat.md)

## Examples

``` r
calc_ar_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 13.9234

calc_ar_sat(
  temp_water = c(5, 15, 25),
  atmo_press = 101.325,
  units = "kPa",
  out_units = "mg/L"
)
#> [1] 0.7789707 0.6163560 0.5059123
```
