# Convert CO2 mole fraction to partial pressure

Converts CO2 mole fraction, `xCO2`, to the water-vapor-corrected partial
pressure, `pCO2`.

## Usage

``` r
xCO2_to_pCO2(xCO2_ppm, temp_water, atmo_press, press_units, ...)
```

## Arguments

- xCO2_ppm:

  Numeric vector. Mole fraction of CO2 in air in parts per million.

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- atmo_press:

  Numeric vector. Atmospheric pressure.

- press_units:

  Character string giving the units of `atmo_press`. See
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  for accepted pressure units.

- ...:

  Additional arguments passed to
  [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md),
  such as `salinity` or `method`.

## Value

Numeric vector of partial pressure of CO2 in microatmospheres.

## Details

The conversion uses: \$\$pCO2 = (P\_{atm} - P\_{H2O}) \cdot xCO2\$\$
where atmospheric pressure and water vapor pressure are in atm and
`xCO2` is supplied in parts per million.

## References

Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3.

## Examples

``` r
xCO2_to_pCO2(
  xCO2_ppm = 420,
  temp_water = 20,
  atmo_press = 101.325,
  press_units = "kPa",
  salinity = 0,
  method = "MIMSY"
)
#> [1] 410.3363
```
