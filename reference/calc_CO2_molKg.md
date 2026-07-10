# Calculate dissolved CO2 concentration in mol/kg (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_co2_mol_kg()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_mol_kg.md)
instead.

## Usage

``` r
calc_CO2_molKg(
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
# calc_CO2_molKg(
#   CO2_ppm = 420,
#   temp_water = 20,
#   water_depth_m = 0.5,
#   atmo_press = 101.325,
#   press_units = "kPa"
# )
# New:
calc_co2_mol_kg(
  co2_ppm = 420,
  temp_water = 20,
  water_depth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 1.606735e-05
```
