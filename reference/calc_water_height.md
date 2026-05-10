# Calculate water height from pressure

Converts pressure readings from vented or unvented water-level sensors
to water height in meters.

## Usage

``` r
calc_water_height(sensor_kPa, atmo_kPa = NULL, water_temp, type = "vented")
```

## Arguments

- sensor_kPa:

  Numeric vector. Sensor pressure in kilopascals. For `type = "vented"`,
  this is differential pressure from the water column. For
  `type = "unvented"`, this is absolute pressure.

- atmo_kPa:

  Numeric vector. Atmospheric pressure in kilopascals. Required when
  `type = "unvented"`.

- water_temp:

  Numeric vector. Water temperature in degrees Celsius.

- type:

  Character string. Sensor type, either `"vented"` or `"unvented"`.
  Defaults to `"vented"`.

## Value

Numeric vector of water height in meters.

## Details

The pressure difference is divided by water density and gravitational
acceleration. Water density is calculated with
[`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md).

## References

Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
liquid water from 0° to 150°C: Correlations and tables for atmospheric
pressure and saturation reviewed and expressed on 1968 temperature
scale. *Journal of Chemical and Engineering Data*, 20(1), 97-105.
[doi:10.1021/je60064a005](https://doi.org/10.1021/je60064a005)

## Examples

``` r
calc_water_height(sensor_kPa = 19.2, water_temp = 15, type = "vented")
#> [1] 1.95962

calc_water_height(
  sensor_kPa = 120.5,
  atmo_kPa = 101.3,
  water_temp = 15,
  type = "unvented"
)
#> [1] 1.95962
```
