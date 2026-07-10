# Calculate dissolved methane saturation (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_ch4_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_ch4_sat.md)
instead.

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

## Examples

``` r
# Old:
# calc_CH4sat(temp_water = 20, atmo_press = 1, salinity = 0, xCH4_ppm = 1.9)
# New:
calc_ch4_sat(temp_water = 20, atmo_press = 1, salinity = 0, xch4_ppm = 1.9)
#> [1] 0.002873362
```
