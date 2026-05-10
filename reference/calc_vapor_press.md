# Calculate water vapor pressure

Calculates the vapor pressure of freshwater or seawater in atmospheres.

## Usage

``` r
calc_vapor_press(temp_water = 25, salinity = 0, method = "Dickson2007")
```

## Arguments

- temp_water:

  Numeric vector. Water temperature in degrees Celsius. Defaults to
  `25`.

- salinity:

  Numeric vector. Salinity in practical salinity units. Defaults to
  freshwater (`0`).

- method:

  Character string. Calculation method. Use `"Dickson2007"` for seawater
  and `"MIMSY"` for freshwater. Defaults to `"Dickson2007"`.

## Value

Numeric vector of vapor pressure in atm.

## Details

`"Dickson2007"` follows the ocean CO2 best-practices guide and includes
a salinity correction. `"MIMSY"` uses the Antoine equation for
freshwater.

## References

Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3.

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
