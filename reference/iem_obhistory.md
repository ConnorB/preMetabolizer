# Get one day of Iowa Environmental Mesonet observations

Retrieves one local calendar day of observations for one IEM station.

## Usage

``` r
iem_obhistory(station, network, date = Sys.Date(), full = FALSE)
```

## Arguments

- station:

  A single station identifier, such as `"DSM"`.

- network:

  A single IEM network identifier, such as `"IA_ASOS"`.

- date:

  A single local calendar date as a `Date` object or `YYYY-MM-DD`
  string. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- full:

  Logical. If `TRUE`, request all available observation fields.

## Value

A tibble containing one day of station observations returned by IEM,
with station identifiers in `station_id` when returned. Units and
available variables vary by network.

## Details

**\[experimental\]**

The `date` argument is interpreted by IEM as a local station calendar
date. UTC timestamps are parsed as `POSIXct` values in the UTC time
zone. Local timestamp fields are returned as character values because
station time zones vary by network.

## References

Iowa Environmental Mesonet API: <https://mesonet.agron.iastate.edu/api/>

IEM API v1 documentation: <https://mesonet.agron.iastate.edu/api/1/docs>

## Examples

``` r
if (FALSE) { # \dontrun{
obs <- iem_obhistory("DSM", network = "IA_ASOS", date = "2024-06-01")
} # }
```
