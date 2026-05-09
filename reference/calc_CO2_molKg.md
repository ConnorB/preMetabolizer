# Calculate CO2 Concentration from Sensor Data

Calculate CO2 Concentration from Sensor Data

## Usage

``` r
calc_CO2_molKg(
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

  Numeric. Mole fraction of CO2 in parts per million.

- temp_water:

  Numeric. Water temperature in degrees Celsius.

- waterDepth_m:

  Numeric. Water depth in meters.

- atmo_press:

  Numeric. Atmospheric pressure.

- press_units:

  Character. Units of atmospheric pressure.

- salinity:

  Numeric. Salinity in PSU.

## Value

a numeric vector of CO2 concentration in mol/kg.
