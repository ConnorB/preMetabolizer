# Calculate Dissolved Oxygen Saturation

This function calculates dissolved oxygen saturation (\[DO\]) in water,
accounting for temperature, barometric pressure, and salinity. It uses
the Benson and Krause \\\mu\\mol/kg coefficients reported by Garcia and
Gordon (1992) and corrects for vapor pressure and water density.

## Usage

``` r
calc_O2sat(temp_water, atmo_press, units = "atm", salinity = 0)
```

## Arguments

- temp_water:

  Numeric. Water temperature in degrees Celsius.

- atmo_press:

  Numeric. Barometric pressure values.

- units:

  Character. Units of barometric pressure. Accepted values are `"atm"`,
  `"hPa"`, `"mbar"`, and `"kPa"`. Default is `"atm"`.

- salinity:

  Numeric. salinity in parts per thousand (ppt). Default is `0`.

## Value

Numeric. Dissolved oxygen saturation (\[DO\]) in mg/L.

## Details

The function calculates dissolved oxygen saturation from the Benson and
Krause \\\mu\\mol/kg fit reported in Garcia and Gordon (1992). It
includes corrections for:

- Barometric pressure, converted from the specified units to atm.

- Vapor pressure of water, using the Antoine equation.

- Water density, dynamically calculated based on temperature and
  salinity.

## Examples

``` r
# Example with mbar
temp <- 25  # Water temperature in °C
atmo_mbar <- c(1013, 1015, 1012)  # Barometric pressure in mbar
calc_O2sat(temp_water = temp, atmo_press = atmo_mbar, units = "mbar")
#> [1] 8.261270 8.278109 8.252851

# Example with kPa
temp <- 20
atmo_kPa <- c(101.3, 101.5, 101.2)  # Barometric pressure in kPa
calc_O2sat(temp_water = temp, atmo_press = atmo_kPa, units = "kPa")
#> [1] 9.090214 9.108583 9.081029
```
