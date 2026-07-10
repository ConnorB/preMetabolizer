# Convert CO2 partial pressure to mole fraction (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`pco2_to_xco2()`](https://connorb.github.io/preMetabolizer/reference/pco2_to_xco2.md)
instead.

## Usage

``` r
pCO2_to_xCO2(temp_water, pCO2_uatm, atmo_press, press_units, ...)
```

## Arguments

- pCO2_uatm:

  **\[deprecated\]** Use `pco2_uatm` instead.

## Examples

``` r
# Old:
# pCO2_to_xCO2(
#   temp_water = 20,
#   pCO2_uatm = 400,
#   atmo_press = 101.325,
#   press_units = "kPa"
# )
# New:
pco2_to_xco2(
  temp_water = 20,
  pco2_uatm = 400,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 409.4526
```
