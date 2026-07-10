# Calculate dissolved oxygen saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_o2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_o2_sat.md)
instead.

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

## Examples

``` r
# Old:
# calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
# New:
calc_o2_sat(temp_water = 15, atmo_press = 1, units = "atm")
#> [1] 10.08393
```
