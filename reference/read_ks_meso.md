# Read cached Kansas Mesonet data

Reads previously downloaded and cached Kansas Mesonet data.

## Usage

``` r
read_ks_meso(station, start_date, end_date, interval, output_dir = NULL)
```

## Arguments

- station:

  Station name as a character string.

- start_date:

  Start date for the data in `YYYY-MM-DD` format.

- end_date:

  End date for the data in `YYYY-MM-DD` format.

- interval:

  Data interval. Must be one of `'hour'`, `'5min'`, or `'day'`.

- output_dir:

  Directory where the cached data is stored. Defaults to the cache path.

## Value

A data frame containing the requested Mesonet data.

## Details

**\[experimental\]**

Kansas Mesonet data are preliminary and subject to revision. Cite the
Kansas Mesonet when sharing, publishing, or otherwise disseminating data
read with this function. A suggested citation format is: Kansas Mesonet,
year: webpage title. Accessed date, webpage URL. Review the Kansas
Mesonet data usage policy before automated use; automated page scraping
or data ingesting without written consent is not permitted.

## References

Kansas Mesonet data usage policy:
<https://mesonet.k-state.edu/about/usage/>

## Examples

``` r
if (FALSE) { # \dontrun{
konza_hourly <- read_ks_meso(
  station = "Konza Prairie",
  start_date = "2024-06-01",
  end_date = "2024-06-07",
  interval = "hour"
)
} # }
```
