# Saturation vapor pressure of water

Computes the saturation vapor pressure of water (or seawater) in atm.

## Usage

``` r
calc_vapor_press(temp_water = 25, salinity = 0, method = "Dickson2007")
```

## Arguments

- temp_water:

  Water temperature in degrees Celsius. May be a vector.

- salinity:

  Practical salinity (unitless). Use 0 for freshwater. Ignored when
  `method = "MIMSY"`.

- method:

  Either "Dickson2007" (Wagner & Pruss pure-water fit with a seawater
  osmotic correction) or "MIMSY" (Antoine equation, freshwater).
  Defaults to `"Dickson2007"`.

## Value

Saturation vapor pressure in atm.

## References

Dickson, A.G., Sabine, C.L. and Christian, J.R. (Eds.) 2007. Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3,
191 pp. (Chapter 5, section 3.) Stull, D.R. 1947. Vapor pressure of pure
substances. Ind. Eng. Chem. 39(4): 517-540. doi:10.1021/ie50448a022
Kelly, M.C. (2024). mimsy: Calculate MIMS Dissolved Gas Concentrations
Without Getting a Headache. R package version 0.6.5.
<https://CRAN.R-project.org/package=mimsy>

## Examples

``` r
calc_vapor_press(temp_water = 25, salinity = 35, method = "Dickson2007")
#> [1] 0.03069887
calc_vapor_press(temp_water = 20, salinity = 0, method = "MIMSY")
#> [1] 0.02300875
```
