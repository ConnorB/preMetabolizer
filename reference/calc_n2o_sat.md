# Calculate dissolved nitrous oxide saturation

Calculates the dissolved nitrous oxide concentration in equilibrium with
the atmosphere from water temperature, barometric pressure, and
salinity, using the Weiss and Price (1980) solubility function for
nitrous oxide as a non-ideal atmospheric trace gas.

## Usage

``` r
calc_n2o_sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xn2o_ppm = 0.338,
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

- xn2o_ppm:

  Numeric vector. Dry-air mole fraction of N2O in parts per million.
  Defaults to `0.338`, an approximate present-day value; override it
  with a measured atmospheric value where available.

- out_units:

  Character string. Output concentration units, either `"umol/L"` (the
  default) or `"mg/L"`.

## Value

Numeric vector of dissolved N2O saturation in the requested units.

## Details

The equilibrium concentration is \\C^\* = x' F\\, where \\x'\\ is the
dry-air mole fraction and \\F\\ is the moist-air solubility function of
Weiss and Price (1980), evaluated with their volumetric coefficients
(Table II). A vapor-pressure correction scales the 1 atm result to the
supplied barometric pressure.

## References

Weiss, R.F., and Price, B.A. (1980). Nitrous oxide solubility in water
and seawater. Marine Chemistry, 8, 347-359.

## See also

[`calc_o2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_o2_sat.md),
[`calc_co2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_sat.md),
[`calc_ch4_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_ch4_sat.md),
[`calc_n2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_n2_sat.md),
[`calc_ar_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_ar_sat.md)

## Examples

``` r
calc_n2o_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 0.009460648

calc_n2o_sat(
  temp_water = c(5, 15, 25),
  atmo_press = 101.325,
  units = "kPa",
  xn2o_ppm = 0.338,
  out_units = "mg/L"
)
#> [1] 0.0007113589 0.0004918582 0.0003560558
```
