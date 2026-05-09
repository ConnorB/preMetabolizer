# Calculate Water Height from Sensor and Atmospheric Pressure

This function computes the water height in meters based on the sensor
pressure readings, atmospheric pressure, water temperature, and sensor
type (vented or unvented).

## Usage

``` r
calc_water_height(sensor_kPa, atmo_kPa = NULL, water_temp, type = "vented")
```

## Arguments

- sensor_kPa:

  Numeric value representing the sensor pressure in kilopascals.

  - For `type = "vented"`, this is the differential pressure (water
    pressure only).

  - For `type = "unvented"`, this is the absolute pressure measured by
    the sensor.

- atmo_kPa:

  Numeric value representing the atmospheric pressure in kilopascals.
  Required when `type = "unvented"`.

- water_temp:

  Numeric value representing the water temperature in degrees Celsius.

- type:

  Character string specifying the sensor type: `"vented"` or
  `"unvented"`. Defaults to `"vented"`.

## Value

A numeric value representing the water height in meters.

## Details

The function calculates the water density using the equation from Kell
(1975) for pure water. It then computes the water height by dividing the
pressure difference by the product of water density and gravity.

- **Vented Sensor (`type = "vented"`):** The sensor measures the
  pressure difference directly, so only `sensor_kPa` is required.

- **Unvented Sensor (`type = "unvented"`):** The sensor measures
  absolute pressure, so atmospheric pressure (`atmo_kPa`) must be
  provided to calculate the pressure difference.

## References

Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
liquid water from 0° to 150°C: Correlations and tables for atmospheric
pressure and saturation reviewed and expressed on 1968 temperature
scale. *Journal of Chemical and Engineering Data*, 20(1), 97–105.
[doi:10.1021/je60064a005](https://doi.org/10.1021/je60064a005)

## Examples

``` r
# Example usage with a vented sensor:
sensor_pressure <- 19.2 # kPa (pressure due to water column)
temperature <- 15 # degrees Celsius
calc_water_height(sensor_pressure, water_temp = temperature, type = "vented")
#> [1] 1.95962

# Example usage with an unvented sensor:
sensor_pressure <- 120.5 # kPa (absolute pressure)
atmospheric_pressure <- 101.3 # kPa
temperature <- 15 # degrees Celsius
calc_water_height(sensor_pressure,
  atmo_kPa = atmospheric_pressure,
  water_temp = temperature, type = "unvented"
)
#> [1] 1.95962
```
