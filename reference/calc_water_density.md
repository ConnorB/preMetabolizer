# Calculate water density

Calculates the density of water (kg/m^3) from temperature in degrees
Celsius and salinity. For freshwater (`salinity = 0`) the density
follows the recommended pure-water (SMOW) equation of Tanaka et al.
(2001) from 0°C to 40°C, falling back to Kell (1975) above 40°C (valid
to 150°C). For seawater (nonzero salinity) it follows the one-atmosphere
international equation of state of Millero and Poisson (1981), valid
from 0°C to 40°C and salinity 0.5 to 43. A warning is issued for
temperatures or salinities outside the valid range of the equation used.

## Usage

``` r
calc_water_density(water_temp, salinity = 0)
```

## Arguments

- water_temp:

  Numeric vector. Water temperature in degrees Celsius.

- salinity:

  Numeric vector. Salinity in parts per thousand. Defaults to freshwater
  (`0`). Any nonzero value uses the Millero and Poisson (1981) seawater
  equation.

## Value

Numeric vector of water density in kg/m^3.

## References

Tanaka, M., Girard, G., Davis, R., Peuto, A., and Bignell, N. (2001).
Recommended table for the density of water between 0°C and 40°C based on
recent experimental reports. *Metrologia*, 38(4), 301–309.
[doi:10.1088/0026-1394/38/4/3](https://doi.org/10.1088/0026-1394/38/4/3)

Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
liquid water from 0° to 150°C: Correlations and tables for atmospheric
pressure and saturation reviewed and expressed on 1968 temperature
scale. *Journal of Chemical and Engineering Data*, 20(1), 97–105.
[doi:10.1021/je60064a005](https://doi.org/10.1021/je60064a005)

Millero, F. J., and Poisson, A. (1981). International one-atmosphere
equation of state of seawater. *Deep-Sea Research Part A*, 28(6),
625–629.
[doi:10.1016/0198-0149(81)90122-9](https://doi.org/10.1016/0198-0149%2881%2990122-9)

## Examples

``` r
calc_water_density(15)
#> [1] 999.1026
calc_water_density(c(0, 10, 20, 30))
#> [1] 999.8428 999.7027 998.2067 995.6488

# Seawater density at salinity 35
calc_water_density(15, salinity = 35)
#> [1] 1025.973
```
