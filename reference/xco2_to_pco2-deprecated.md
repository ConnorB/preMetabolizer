# Convert CO2 mole fraction to partial pressure (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`xco2_to_pco2()`](https://connorb.github.io/preMetabolizer/reference/xco2_to_pco2.md)
instead.

## Usage

``` r
xCO2_to_pCO2(xCO2_ppm, temp_water, atmo_press, press_units, ...)
```

## Arguments

- xCO2_ppm:

  **\[deprecated\]** Use `xco2_ppm` instead.

## Examples

``` r
# Old:
# xCO2_to_pCO2(
#   xCO2_ppm = 420,
#   temp_water = 20,
#   atmo_press = 101.325,
#   press_units = "kPa"
# )
# New:
xco2_to_pco2(
  xco2_ppm = 420,
  temp_water = 20,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 410.3039
```
