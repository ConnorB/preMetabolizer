# Calculate dissolved carbon dioxide saturation

Calculates the dissolved carbon dioxide concentration in equilibrium
with the atmosphere from water temperature, barometric pressure, and
salinity, using the Weiss and Price (1980) solubility function for
carbon dioxide as a non-ideal atmospheric trace gas.

## Usage

``` r
calc_CO2sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xCO2_ppm = 420,
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

- xCO2_ppm:

  Numeric vector. Dry-air mole fraction of CO2 in parts per million.
  Defaults to `420`, an approximate present-day value; override it with
  a measured atmospheric value where available.

- out_units:

  Character string. Output concentration units, either `"umol/L"` (the
  default) or `"mg/L"`.

## Value

Numeric vector of dissolved CO2 saturation in the requested units.

## Details

The equilibrium concentration is \\C^\* = x' F\\, where \\x'\\ is the
dry-air mole fraction and \\F\\ is the moist-air solubility function of
Weiss and Price (1980), evaluated with their volumetric coefficients
(Table VI). A vapor-pressure correction scales the 1 atm result to the
supplied barometric pressure. To convert a *measured* CO2 mole fraction
(rather than the atmospheric value) to a dissolved concentration, see
[`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md).

## References

Weiss, R.F., and Price, B.A. (1980). Nitrous oxide solubility in water
and seawater. Marine Chemistry, 8, 347-359.

## See also

[`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md),
[`calc_CH4sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CH4sat.md),
[`calc_N2Osat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2Osat.md),
[`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)

## Examples

``` r
calc_CO2sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 15.99144

calc_CO2sat(
  temp_water = c(5, 15, 25),
  atmo_press = 101.325,
  units = "kPa",
  xCO2_ppm = 420,
  out_units = "mg/L"
)
#> [1] 1.1690779 0.8243273 0.6064212
```
