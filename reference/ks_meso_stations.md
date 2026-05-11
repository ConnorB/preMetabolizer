# Get Kansas Mesonet station information

Fetches metadata about Kansas Mesonet stations, including location and
network details.

## Usage

``` r
ks_meso_stations()
```

## Value

A tibble containing station metadata, including `station_name`,
`station_id`, `network`, and `network_name`.

## Details

**\[experimental\]**

Kansas Mesonet data are preliminary and subject to revision. Cite the
Kansas Mesonet when sharing, publishing, or otherwise disseminating data
accessed with this function. A suggested citation format is: Kansas
Mesonet, year: webpage title. Accessed date, webpage URL. Review the
Kansas Mesonet data usage policy before automated use; automated page
scraping or data ingesting without written consent is not permitted.

## References

Kansas Mesonet data usage policy:
<https://mesonet.k-state.edu/about/usage/>

## Examples

``` r
if (FALSE) { # \dontrun{
stations <- ks_meso_stations()
subset(stations, grepl("Konza", station_name))
} # }
```
