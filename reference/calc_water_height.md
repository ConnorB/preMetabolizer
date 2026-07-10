# Calculate water height from pressure

Converts pressure readings from vented or unvented water-level sensors
to water height in meters.

## Usage

``` r
calc_water_height(
  sensor_kpa,
  atmo_kpa = NULL,
  water_temp,
  type = "vented",
  sensor_kPa = lifecycle::deprecated(),
  atmo_kPa = lifecycle::deprecated()
)
```

## Arguments

- sensor_kpa:

  Numeric vector. Sensor pressure in kilopascals. For `type = "vented"`,
  this is differential pressure from the water column. For
  `type = "unvented"`, this is absolute pressure.

- atmo_kpa:

  Numeric vector. Atmospheric pressure in kilopascals. Required when
  `type = "unvented"`.

- water_temp:

  Numeric vector. Water temperature in degrees Celsius.

- type:

  Character string. Sensor type, either `"vented"` or `"unvented"`.
  Defaults to `"vented"`.

- sensor_kPa:

  **\[deprecated\]** Use `sensor_kpa` instead.

- atmo_kPa:

  **\[deprecated\]** Use `atmo_kpa` instead.

## Value

Numeric vector of water height in meters.

## Details

The pressure difference is divided by water density and gravitational
acceleration. Water density is calculated with
[`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md).

## References

Tanaka, M., Girard, G., Davis, R., Peuto, A., and Bignell, N. (2001).
Recommended table for the density of water between 0°C and 40°C based on
recent experimental reports. *Metrologia*, 38(4), 301-309.
[doi:10.1088/0026-1394/38/4/3](https://doi.org/10.1088/0026-1394/38/4/3)

## Examples

``` r
calc_water_height(sensor_kpa = 19.2, water_temp = 15, type = "vented")
#> [1] 1.959614

calc_water_height(
  sensor_kpa = 120.5,
  atmo_kpa = 101.3,
  water_temp = 15,
  type = "unvented"
)
#> [1] 1.959614
```
