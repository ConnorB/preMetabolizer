# Calculate the vapor pressure of water at the supplied conditions

This function calculates the vapor pressure of fresh or sea water.

## Usage

``` r
calc_vapor_press(temp_water = 25, salinity = 0, method = "Dickson2007")
```

## Arguments

- temp_water:

  Water temperature in degrees Celsius (default: 25).

- salinity:

  Salinity of the water in PSU (default: 0).

- method:

  Character. The method to use for calculation, either `"Dickson2007"`
  for seawater or `"MIMSY"` for freshwater. Defaults to `"Dickson2007"`.

## Value

Vapor pressure in atm.

## Details

- `"Dickson2007"` follows the method outlined in Dickson et al. (2007)
  and adjusts for seawater.

- `"MIMSY"` uses the Antoine equation for freshwater vapor pressure
  calculation.

## References

Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3,
191 pp.

Kelly, M.C. (2024). mimsy: Calculate MIMS Dissolved Gas Concentrations
Without Getting a Headache. R package version 0.6.5. Available at:
<https://CRAN.R-project.org/package=mimsy>

## Examples

``` r
calc_vapor_press(temp_water = 25, salinity = 35,  method = "Dickson2007")
#> [1] 0.03069887
calc_vapor_press(temp_water = 20, salinity = 0, method = "MIMSY")
#> [1] 0.02300875
```
