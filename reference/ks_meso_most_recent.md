# Get Most Recent Kansas Mesonet Data Timestamp

Retrieves the timestamp of the most recently ingested data for each
station for a given observation interval. The returned data includes the
`"--all--"` row reported by Kansas Mesonet.

## Usage

``` r
ks_meso_most_recent(interval)
```

## Arguments

- interval:

  Data interval. Must be one of `"hour"`, `"5min"`, or `"day"`.

## Value

A data frame with station names and most recent observation times.

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
