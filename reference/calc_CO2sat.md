# Calculate dissolved carbon dioxide saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_co2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_sat.md)
instead.

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

## Examples

``` r
# Old:
# calc_CO2sat(temp_water = 20, atmo_press = 1, salinity = 0, xCO2_ppm = 420)
# New:
calc_co2_sat(temp_water = 20, atmo_press = 1, salinity = 0, xco2_ppm = 420)
#> [1] 15.99144
```
