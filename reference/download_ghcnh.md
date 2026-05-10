# Download GHCNh CSV files

Downloads GHCNh CSV files for specified stations and years, saving them
to an output directory.

## Usage

``` r
download_ghcnh(station_id, years, output_dir = NULL, quiet = FALSE)
```

## Arguments

- station_id:

  Character string specifying the station ID for which data is to be
  downloaded.

- years:

  Numeric vector specifying the years for which data is to be
  downloaded.

- output_dir:

  Optional. Character string specifying the output directory. Defaults
  to NOAA cache directory.

- quiet:

  Logical. If `TRUE`, suppresses messages. Defaults to `FALSE`.

## Value

A list summarizing the download process:

- output_directory:

  The directory where files were saved.

- successful_downloads:

  Character vector of successfully downloaded files.

- skipped_downloads:

  Character vector of files already present locally.

- failed_downloads:

  Character vector of failed downloads.

- total_attempted:

  Total number of download attempts.

- total_successful:

  Total number of successful downloads.

## Examples

``` r
if (FALSE) { # \dontrun{
  download_ghcnh("USW00023183", 2020:2022, output_dir = "data/ghcnh")
} # }
```
