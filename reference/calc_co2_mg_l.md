# Calculate dissolved CO2 concentration in mg/L

Converts a measured or modeled CO2 mole fraction to dissolved CO2
concentration in mg/L. This is a volume-based companion to
[`calc_co2_mol_kg()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_mol_kg.md)
and uses water density to convert from mol/kg.

## Usage

``` r
calc_co2_mg_l(
  co2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0
)
```

## Arguments

- co2_ppm:

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

## Value

Numeric vector of dissolved CO2 concentration in mg/L.

## See also

[`calc_co2_mol_kg()`](https://connorb.github.io/preMetabolizer/reference/calc_co2_mol_kg.md),
[`xco2_to_pco2()`](https://connorb.github.io/preMetabolizer/reference/xco2_to_pco2.md),
[`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)

## Examples

``` r
calc_co2_mg_l(
  co2_ppm = c(420, 800, 1200),
  temp_water = 20,
  water_depth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
#> [1] 0.7058479 1.3444723 2.0167084
```
