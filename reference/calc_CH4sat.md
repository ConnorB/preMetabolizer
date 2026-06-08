# Calculate dissolved methane saturation

Calculates the dissolved methane concentration in equilibrium with the
atmosphere from water temperature, barometric pressure, and salinity,
using the atmospheric-equilibrium solubility equation of Wiesenburg and
Guinasso (1979).

## Usage

``` r
calc_CH4sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xCH4_ppm = 1.9,
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

- xCH4_ppm:

  Numeric vector. Dry-air mole fraction of CH4 in parts per million.
  Defaults to `1.9`, an approximate present-day value; override it with
  a measured atmospheric value where available.

- out_units:

  Character string. Output concentration units, either `"umol/L"` (the
  default) or `"mg/L"`.

## Value

Numeric vector of dissolved CH4 saturation in the requested units.

## Details

The equilibrium concentration follows Wiesenburg and Guinasso (1979) eq
7 with their volumetric (nmol/L) coefficients (Table VI), in which the
dry-air mole fraction enters the solubility equation directly. A
vapor-pressure correction scales the 1 atm result to the supplied
barometric pressure.

## References

Wiesenburg, D.A., and Guinasso, N.L. (1979). Equilibrium solubilities of
methane, carbon monoxide, and hydrogen in water and sea water. Journal
of Chemical and Engineering Data, 24(4), 356-360.

## See also

[`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md),
[`calc_CO2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2sat.md),
[`calc_N2Osat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2Osat.md)

## Examples

``` r
calc_CH4sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 0.002873362

calc_CH4sat(
  temp_water = c(5, 15, 25),
  atmo_press = 101.325,
  units = "kPa",
  xCH4_ppm = 1.9,
  out_units = "mg/L"
)
#> [1] 6.673778e-05 5.155430e-05 4.155221e-05
```
