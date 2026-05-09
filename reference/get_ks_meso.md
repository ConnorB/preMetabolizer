# Fetch Data from Kansas Mesonet

Retrieves weather data for specified stations from the Kansas Mesonet.

## Usage

``` r
get_ks_meso(
  stations,
  start_date,
  end_date,
  interval,
  vars = NULL,
  output_dir = NULL,
  debug = T
)
```

## Arguments

- stations:

  Character vector of station names to retrieve data for.

- start_date:

  Start date for the data retrieval in `YYYY-MM-DD` format.

- end_date:

  End date for the data retrieval in `YYYY-MM-DD` format.

- interval:

  Data interval. Must be one of `'hour'`, `'5min'`, or `'day'`.

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
