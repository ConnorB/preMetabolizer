# Calculate dissolved nitrogen saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_n2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_n2_sat.md)
instead.

## Usage

``` r
calc_N2sat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  out_units = "umol/L"
)
```

## Examples

``` r
# Old:
# calc_N2sat(temp_water = 20, atmo_press = 1, salinity = 0)
# New:
calc_n2_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 536.4795
```
