# Get Kansas Mesonet station activity

Retrieves activity data for Kansas Mesonet stations, including
observation intervals and data spans.

## Usage

``` r
ks_meso_station_activity()
```

## Value

A data frame with station activity details, including start and end
observation times.

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
activity <- ks_meso_station_activity()
subset(activity, station == "Konza Prairie")
} # }
```
