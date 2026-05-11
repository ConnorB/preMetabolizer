# Get Iowa Environmental Mesonet station metadata

Retrieves station metadata for one IEM network or one station
identifier.

## Usage

``` r
iem_stations(network)

iem_station(station_id)
```

## Arguments

- network:

  A single IEM network identifier, such as `"IA_ASOS"`.

- station_id:

  A single station identifier, such as `"DSM"`.

## Value

A tibble containing station metadata returned by IEM, including station
identifiers in `station_id`, names in `station_name`, network
identifiers, time zones, archive dates, and longitude and latitude when
available.

## Details

**\[experimental\]**

`iem_stations()` returns all stations in one network. `iem_station()`
looks up one station identifier across IEM networks, which is useful
because some identifiers are shared by more than one network.

## References

Iowa Environmental Mesonet API: <https://mesonet.agron.iastate.edu/api/>

IEM API v1 documentation: <https://mesonet.agron.iastate.edu/api/1/docs>

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- iem_stations("IA_ASOS")
iem_station("DSM")
} # }
```
