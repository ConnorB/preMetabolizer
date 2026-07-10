# Calculate dissolved CO2 concentration in mg/L (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_co2_mg_l()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_mg_l.md)
instead.

## Usage

``` r
calc_CO2_mgL(
  CO2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0,
  waterDepth_m = lifecycle::deprecated()
)
```

## Arguments

- CO2_ppm:

  **\[deprecated\]** Use `co2_ppm` instead.

- waterDepth_m:

  **\[deprecated\]** Use `water_depth_m` instead.

## Examples

``` r
# Old:
# calc_CO2_mgL(
#   CO2_ppm = c(420, 800, 1200),
#   temp_water = 20,
#   water_depth_m = 0.5,
#   atmo_press = 101.325,
#   press_units = "kPa"
# )
# New:
calc_co2_mg_l(
  co2_ppm = c(420, 800, 1200),
  temp_water = 20,
  water_depth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 0.7058479 1.3444723 2.0167084
```
