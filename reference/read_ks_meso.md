# Read Cached Kansas Mesonet Data

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
