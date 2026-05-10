# Convert CO2 partial pressure to mole fraction

Converts water-vapor-corrected CO2 partial pressure, `pCO2`, to CO2 mole
fraction, `xCO2`.

## Usage

``` r
pCO2_to_xCO2(temp_water, pCO2_uatm, atmo_press, press_units, ...)
```

## Arguments

- temp_water:

  Numeric vector. Water temperature in degrees Celsius.

- pCO2_uatm:

  Numeric vector. Partial pressure of CO2 in microatmospheres.

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

Numeric vector of CO2 mole fraction in parts per million.

## References

Dickson, A.G., Sabine, C.L., and Christian, J.R. (Eds.) (2007). Guide to
best practices for ocean CO2 measurements. PICES Special Publication 3.

## Examples

``` r
pCO2_to_xCO2(
  temp_water = 20,
  pCO2_uatm = 400,
  atmo_press = 101.325,
  press_units = "kPa",
  salinity = 0,
  method = "MIMSY"
)
#> [1] 409.4202
```
