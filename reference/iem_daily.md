# Get Iowa Environmental Mesonet daily summaries

Retrieves daily summary observations from the Iowa Environmental Mesonet
(IEM).

## Usage

``` r
iem_daily(network, station = NULL, date = NULL, year = NULL, month = NULL)
```

## Arguments

- network:

  A single IEM network identifier, such as `"IA_ASOS"`.

- station:

  Optional single station identifier, such as `"DSM"`.

- date:

  Optional single local calendar date as a `Date` object or `YYYY-MM-DD`
  string.

- year:

  Optional whole number year.

- month:

  Optional whole number month from 1 to 12. Requires `year`.

## Value

A tibble containing daily summary observations returned by IEM, with
station identifiers in `station_id` when returned. Units and available
variables vary by network.

## Details

**\[experimental\]**

Use either `date` for all stations in a network on one local date, or
`year` and optional `month` for a longer station or network request. Do
not supply `date` together with `year` or `month`.

## References

Iowa Environmental Mesonet API: <https://mesonet.agron.iastate.edu/api/>

IEM API v1 documentation: <https://mesonet.agron.iastate.edu/api/1/docs>

## Examples

``` r
if (FALSE) { # \dontrun{
daily <- iem_daily("IA_ASOS", station = "DSM", year = 2024)
network_day <- iem_daily("IA_ASOS", date = "2024-06-01")
} # }
```
