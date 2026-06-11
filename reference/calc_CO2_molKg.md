# Calculate dissolved CO2 concentration in mol/kg

Converts a measured or modeled CO2 mole fraction to dissolved CO2
concentration using atmospheric pressure, water-column pressure, vapor
pressure, and the Weiss (1974) CO2 solubility coefficient.

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

  Numeric vector. Mole fraction of CO2 in air in parts per million.

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- water_depth_m:

  Numeric vector. Water depth above the sensor in meters.

- atmo_press:

  Numeric vector. Atmospheric pressure at the water surface.

- press_units:

  Character string giving the units of `atmo_press`. See
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  for accepted pressure units.

- salinity:

  Numeric vector. Salinity in practical salinity units. Defaults to
  freshwater (`0`).

- waterDepth_m:

  **\[deprecated\]** Use `water_depth_m` instead.

## Value

Numeric vector of dissolved CO2 concentration in mol/kg.

## See also

[`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md),
[`xCO2_to_pCO2()`](https://connorb.github.io/preMetabolizer/reference/xCO2_to_pCO2.md),
[`calc_K0()`](https://connorb.github.io/preMetabolizer/reference/calc_K0.md)

## Examples

``` r
calc_CO2_molKg(
  CO2_ppm = 420,
  temp_water = 20,
  water_depth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 1.606735e-05
```
