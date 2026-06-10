# Calculate dissolved CO2 concentration in mg/L

Converts a measured or modeled CO2 mole fraction to dissolved CO2
concentration in mg/L. This is a volume-based companion to
[`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
and uses water density to convert from mol/kg.

## Usage

``` r
calc_CO2_mgL(
  CO2_ppm,
  temp_water,
  waterDepth_m,
  atmo_press,
  press_units,
  salinity = 0
)
```

## Arguments

- CO2_ppm:

  Numeric vector. Mole fraction of CO2 in air in parts per million.

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- waterDepth_m:

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

## Value

Numeric vector of dissolved CO2 concentration in mg/L.

## See also

[`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md),
[`xCO2_to_pCO2()`](https://connorb.github.io/preMetabolizer/reference/xCO2_to_pCO2.md),
[`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)

## Examples

``` r
calc_CO2_mgL(
  CO2_ppm = c(420, 800, 1200),
  temp_water = 20,
  waterDepth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 0.7058479 1.3444723 2.0167084
```
