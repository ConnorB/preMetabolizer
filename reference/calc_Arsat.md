# Calculate dissolved argon saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_ar_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_ar_sat.md)
instead.

## Usage

``` r
calc_Arsat(
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
# calc_Arsat(temp_water = 20, atmo_press = 1, salinity = 0)
# New:
calc_ar_sat(temp_water = 20, atmo_press = 1, salinity = 0)
#> [1] 13.9234
```
