# Get NOAA global monthly mean greenhouse gas concentrations

Downloads the NOAA Global Monitoring Laboratory (GML) globally averaged
marine surface monthly mean records for carbon dioxide, methane, nitrous
oxide, and sulphur hexafluoride. By default each gas is returned in its
standard NOAA reporting unit; set `units` to express every gas on a
common scale.

## Usage

``` r
get_noaa_ghg(gas = c("co2", "ch4", "n2o", "sf6"), units = NULL, quiet = FALSE)
```

## Source

<https://gml.noaa.gov/ccgg/trends/>

## Arguments

- gas:

  Character vector naming one or more gases to download. Any of `"co2"`,
  `"ch4"`, `"n2o"`, and `"sf6"` (case insensitive). Defaults to all
  four.

- units:

  Character string giving the output mole-fraction unit, one of `"ppm"`,
  `"ppb"`, or `"ppt"` (case insensitive). When `NULL` (the default) each
  gas is returned in its standard NOAA reporting unit (CO2 in ppm, CH4
  and N2O in ppb, SF6 in ppt); otherwise every gas is converted to the
  requested unit.

- quiet:

  Logical. When `TRUE` progress messages are suppressed. Default
  `FALSE`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per gas-month and the columns:

- `gas`:

  Gas name in upper case (`"CO2"`, `"CH4"`, `"N2O"`, `"SF6"`).

- `date`:

  Month of the observation as a `POSIXct`, centred on the middle of the
  month (NOAA's decimal date).

- `unit`:

  Mole-fraction unit of the value columns (`"ppm"`, `"ppb"`, or
  `"ppt"`).

- `average`:

  Globally averaged monthly mean mole fraction.

- `average_unc`:

  Uncertainty of the monthly mean.

- `trend`:

  De-seasonalised trend value.

- `trend_unc`:

  Uncertainty of the trend value.

NOAA reports a negative sentinel (`-9.99`) when an uncertainty has not
yet been calculated; these are returned as `NA`.

## Details

**\[experimental\]**

NOAA reports each gas in a different native unit (CO2 in ppm, CH4 and
N2O in ppb, and SF6 in ppt). By default these native units are preserved
and recorded in the `unit` column. When `units` is supplied, every gas
is rescaled to that common unit.

The data presented for the most recent year are preliminary and subject
to change as reference gases are recalibrated.

These data are made freely available by NOAA GML. When the data are
central to a publication, please cite NOAA GML and consider contacting
the data providers, who can advise on appropriate use and
acknowledgement.

## References

Lan, X., Tans, P., & K.W. Thoning: Trends in globally-averaged
greenhouse gases determined from NOAA Global Monitoring Laboratory
measurements. <https://gml.noaa.gov/ccgg/trends/>

Dlugokencky, E. J., et al. (1994). The growth rate and distribution of
atmospheric methane. *Journal of Geophysical Research*, 99(D8),
17021–17043. [doi:10.1029/94JD01245](https://doi.org/10.1029/94JD01245)

## Examples

``` r
if (FALSE) { # \dontrun{
# All four gases in their standard units, stacked long
ghg <- get_noaa_ghg()

# Convert every gas to ppb
ghg_ppb <- get_noaa_ghg(units = "ppb")

# A single gas
co2 <- get_noaa_ghg("co2")
} # }
```
