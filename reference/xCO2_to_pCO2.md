# Convert Mole Fraction of CO2 (xCO2) to Partial Pressure (pCO2)

This function calculates the partial pressure of carbon dioxide (pCO2)
in water from its mole fraction (xCO2) in air, considering temperature
and atmospheric pressure.

## Usage

``` r
xCO2_to_pCO2(xCO2_ppm, temp_water, atmo_press, press_units, ...)
```

## Arguments

- xCO2_ppm:

  Numeric. Mole fraction of CO2 in air in parts per million (ppm).
  Default is 400 ppm.

- temp_water:

  Numeric. Water temperature in degrees Celsius.

- atmo_press:

  Numeric. Atmospheric pressure.

- press_units:

  Character. Units of atmospheric pressure.

- ...:

  Additional arguments passed to the `calc_vapor_press` function.

## Value

Numeric. Partial pressure of CO2 in µatm.

## Details

The partial pressure of CO2 (\\pCO2\\) is computed using the equation:
\$\$pCO2 = (P\_{atm} - P\_{H2O}) \cdot xCO2\$\$ where \\P\_{atm}\\ is
the atmospheric pressure, \\P\_{H2O}\\ is the water vapor pressure, and
\\xCO2\\ is the mole fraction of CO2 in air.

## References

- Dickson, A.G., Sabine, C.L., & Christian, J.R. (2007). *Guide to Best
  Practices for Ocean CO2 Measurements*.

- Weiss, R.F. (1974). Carbon dioxide in water and seawater: the
  solubility of a non-ideal gas. Marine Chemistry, 2(3), 203–215.
