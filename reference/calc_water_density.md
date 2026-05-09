# Calculate Water Density

Calculates the density of pure water (kg/m^3) from temperature in
degrees Celsius using the polynomial approximation of Kell (1975). Valid
for temperatures between 0°C and 150°C at atmospheric pressure.

## Usage

``` r
calc_water_density(water_temp)
```

## Arguments

- water_temp:

  Numeric vector. Water temperature in degrees Celsius.

## Value

Numeric vector of water density in kg/m^3.

## References

Kell, G. S. (1975). Density, thermal expansivity, and compressibility of
liquid water from 0° to 150°C: Correlations and tables for atmospheric
pressure and saturation reviewed and expressed on 1968 temperature
scale. *Journal of Chemical and Engineering Data*, 20(1), 97–105.
[doi:10.1021/je60064a005](https://doi.org/10.1021/je60064a005)

## Examples

``` r
calc_water_density(15)
#> [1] 999.0996
calc_water_density(c(0, 10, 20, 30))
#> [1] 999.8395 999.6996 998.2041 995.6473
```
