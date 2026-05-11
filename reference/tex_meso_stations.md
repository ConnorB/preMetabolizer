# Get TexMesonet station information

Retrieves name, location, and status metadata for Texas Water
Development Board (TWDB) stations from the TexMesonet API.

## Usage

``` r
tex_meso_stations(active = NULL, displayed = NULL)
```

## Arguments

- active:

  Optional logical value used to filter stations by active status. If
  `NULL` (default), active and inactive stations are returned.

- displayed:

  Optional logical value used to filter stations by whether they are
  displayed by TexMesonet. If `NULL` (default), displayed and hidden
  stations are returned.

## Value

A tibble containing station metadata returned by TexMesonet, including
`station_id`, `station_name`, display ID, state, county, latitude,
longitude, elevation in feet, active status, and online date.

## Details

**\[experimental\]**

TexMesonet is managed by the Texas Water Development Board. Its public
API provides near-real-time data for TWDB weather and soil observing
stations. Review the TexMesonet disclaimer before automated use.

## References

TexMesonet APIs: <https://www.texmesonet.org/Apis>

TexMesonet disclaimer: <https://www.texmesonet.org/About>

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- tex_meso_stations(active = TRUE)
subset(stations, county == "Blanco")
} # }
```
