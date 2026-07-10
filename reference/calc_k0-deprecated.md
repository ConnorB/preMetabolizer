# Calculate the CO2 solubility coefficient (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`calc_k0()`](https://connorb.github.io/preMetabolizer/reference/calc_k0.md)
instead.

## Usage

``` r
calc_K0(
  temp_water,
  water_depth_m = 0,
  atmo_press = 1,
  salinity = 0,
  waterDepth_m = lifecycle::deprecated()
)
```

## Arguments

- waterDepth_m:

  **\[deprecated\]** Use `water_depth_m` instead.

## Examples

``` r
# Old:
# calc_K0(temp_water = 20)
# New:
calc_k0(temp_water = 20)
#> [1] 0.03916223
```
