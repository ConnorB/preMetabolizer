# Get Iowa Environmental Mesonet network identifiers

Retrieves the network table from the Iowa Environmental Mesonet (IEM)
API.

## Usage

``` r
iem_networks()
```

## Value

A tibble containing IEM network identifiers in `network`, network names
in `network_name`, time zones, geographic extents, and windrose update
timestamps when available.

## Details

**\[experimental\]**

The Iowa Environmental Mesonet groups stations into networks such as
`IA_ASOS`, `IA_COOP`, and other state, roadway, hydrological, and
special observing networks. Use `iem_networks()` to discover valid
`network` values for other IEM helpers.

## References

Iowa Environmental Mesonet API: <https://mesonet.agron.iastate.edu/api/>

IEM API v1 documentation: <https://mesonet.agron.iastate.edu/api/1/docs>

## Examples

``` r
if (FALSE) { # \dontrun{
networks <- iem_networks()
subset(networks, id == "IA_ASOS")
} # }
```
