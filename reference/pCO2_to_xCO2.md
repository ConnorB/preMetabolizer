# Convert p(CO2) to x(CO2)

Converts partial pressure of CO2 p(CO2) µatm to mole fraction of CO2
x(CO2) ppm. This involves corrections for water vapor pressure to
account for equilibrium conditions.

## Usage

``` r
pCO2_to_xCO2(temp_water, pCO2_uatm, atmo_press, press_units, ...)
```

## Arguments

- temp_water:

  Water temperature in degrees Celsius.

- pCO2_uatm:

  Partial pressure of CO2 in µatm.

- atmo_press:

  Atmospheric pressure.

- press_units:

  Units for pressure, either "atm" or others (default: "atm").

- ...:

  Additional parameters passed to `calc_vapor_press`.

## Value

CO2 mole fraction (xCO2) in ppm.

## References

Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3,
191 pp.
