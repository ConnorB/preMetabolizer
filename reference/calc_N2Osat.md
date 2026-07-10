# Calculate dissolved nitrous oxide saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_n2o_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_n2o_sat.md)
instead.

## Usage

``` r
calc_N2Osat(
  temp_water,
  atmo_press,
  units = "atm",
  salinity = 0,
  xN2O_ppm = 0.338,
  out_units = "umol/L"
)
```

## Examples

``` r
# Old:
# calc_N2Osat(temp_water = 20, atmo_press = 1, salinity = 0, xN2O_ppm = 0.338)
# New:
calc_n2o_sat(temp_water = 20, atmo_press = 1, salinity = 0, xn2o_ppm = 0.338)
#> [1] 0.009460648
```
