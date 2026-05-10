# Read GHCNh CSV Files

Reads GHCNh CSV files from specified files or directory and optionally
combines them into a single dataframe.

## Usage

``` r
read_ghcnh(files = NULL, directory = NULL, combine = TRUE)
```

## Arguments

- files:

  Optional. Character vector specifying the paths to CSV files. Defaults
  to `NULL`.

- directory:

  Optional. Character string specifying the directory containing CSV
  files. Defaults to `NULL`.

- combine:

  Logical. If `TRUE`, combines all successfully read files into a single
  dataframe. Defaults to `TRUE`.

## Value

If `combine` is `TRUE`, returns a list with:

- data:

  Combined dataframe of all successfully read files.

- files_read:

  Character vector of successfully read files.

- files_failed:

  Character vector of failed reads.

- total_rows:

  Total number of rows in the combined dataframe.

- date_range:

  Date range of the combined data.

- stations:

  Unique stations in the combined data.

- removed_cols:

  List of columns removed due to all NA values.

If `combine` is `FALSE`, returns a list of individual dataframes and
read results.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read files from a directory
  data <- read_ghcnh(directory = "data/ghcnh")

  # Read specific files
  files <- c("data/ghcnh/GHCNh_USW00023183_2022.csv.gz",
             "data/ghcnh/GHCNh_USW00023183_2023.csv.gz")
  data <- read_ghcnh(files = files)
} # }
```
