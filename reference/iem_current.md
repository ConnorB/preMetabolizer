# Get current Iowa Environmental Mesonet observations

Retrieves current observations from the Iowa Environmental Mesonet
(IEM).

## Usage

``` r
iem_current(
  network = NULL,
  networkclass = NULL,
  wfo = NULL,
  country = NULL,
  state = NULL,
  stations = NULL,
  minutes = NULL
)
```

## Arguments

- network:

  Optional single IEM network identifier, such as `"IA_ASOS"`.

- networkclass:

  Optional single network class, such as `"ASOS"` or `"COOP"`.

- wfo:

  Optional single National Weather Service forecast office identifier,
  such as `"DMX"`.

- country:

  Optional single country identifier, such as `"US"`.

- state:

  Optional single state identifier, such as `"IA"`.

- stations:

  Optional character vector of station identifiers.

- minutes:

  Optional whole number giving the maximum observation age in minutes.

## Value

A tibble containing current station metadata and observations returned
by IEM, with station identifiers in `station_id`. Units and available
variables vary by network.

## Details

**\[experimental\]**

At least one filter must be supplied. Station identifiers may be shared
across networks, so a station-only request can return multiple rows for
a single identifier. Use `network` with `stations` when you need one
network.

## References

Iowa Environmental Mesonet API: <https://mesonet.agron.iastate.edu/api/>

IEM API v1 documentation: <https://mesonet.agron.iastate.edu/api/1/docs>

## Examples

``` r
if (FALSE) { # \dontrun{
current <- iem_current(network = "IA_ASOS")
dsm <- iem_current(network = "IA_ASOS", stations = "DSM")
} # }
```
