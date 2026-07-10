# Calculate the CO2 solubility coefficient

Calculates the Weiss (1974) CO2 solubility coefficient, `K0`, for water
at the supplied temperature, salinity, atmospheric pressure, and water
depth. The depth and atmospheric pressure terms apply a pressure
correction for the total pressure at the sensor.

## Usage

``` r
calc_k0(temp_water, water_depth_m = 0, atmo_press = 1, salinity = 0)
```

## Arguments

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- water_depth_m:

  Numeric vector. Water depth in meters. Defaults to `0`.

- atmo_press:

  Numeric vector. Atmospheric pressure in atm. Defaults to standard
  atmosphere (`1`).

- salinity:

  Numeric vector. Salinity in practical salinity units. Defaults to
  freshwater (`0`).

## Value

Numeric vector of `K0` values in mol/(kg atm).

## References

Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility
of a non-ideal gas. Marine Chemistry, 2, 203-215.

## Examples

``` r
calc_k0(temp_water = 20)
#> [1] 0.03916223
calc_k0(temp_water = c(5, 15, 25), water_depth_m = 1, salinity = 0)
#> [1] 0.06406044 0.04555376 0.03405660
```
