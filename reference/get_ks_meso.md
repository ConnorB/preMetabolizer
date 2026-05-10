# Fetch Data from Kansas Mesonet

Retrieves weather data for specified stations from the Kansas Mesonet.

## Usage

``` r
get_ks_meso(
  stations = NULL,
  network = NULL,
  start_date,
  end_date,
  interval,
  vars = NULL,
  output_dir = NULL,
  debug = TRUE
)
```

## Arguments

- stations:

  Character vector of station names to retrieve data for. Use `"all"` to
  retrieve data for all stations. Must be `NULL` when `network` is
  supplied.

- network:

  Network name to retrieve data for, one of:

  - `"BBW"`: Big Bend Groundwater Management District

  - `"EBW"`: Equus Beds Groundwater Management District

  - `"KSRE"`: K-State Research and Extension

  This is an alternative to `stations`.

- start_date:

  Start date for the data retrieval in `YYYY-MM-DD` format.

- end_date:

  End date for the data retrieval in `YYYY-MM-DD` format.

- interval:

  Data interval. Must be one of `"hour"`, `"5min"`, or `"day"`.

- vars:

  Character vector of variables to retrieve. Defaults to common
  variables.

- output_dir:

  Directory to save the downloaded data. Defaults to the cache path.

- debug:

  Logical; if `TRUE`, debug messages are printed.

## Value

A list with details about successful and failed downloads, output
directory, and data chunks.

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
