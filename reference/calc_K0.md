# Calculate Solubility Constant (K0) for CO2 in Water

This function calculates the solubility constant (K0) for CO2 in water
as a function of temperature and salinity, based on Weiss (1974).

## Usage

``` r
calc_K0(temp_water, waterDepth_m = 0, atmo_press = 1, salinity = 0)
```

## Arguments

- temp_water:

  Numeric. Water temperature in degrees Celsius.

- waterDepth_m:

  Numeric. Water depth in meters.

- atmo_press:

  Numeric. Atmospheric pressure in atm.

- salinity:

  Numeric. Salinity in PSU (Practical Salinity Units).

## Value

Numeric. Solubility constant (K0) in mol/(kg·atm).

## References

Weiss, R.F. (1974). Carbon dioxide in water and seawater: the solubility
of a non-ideal gas. Marine Chemistry, 2, 203-215.
