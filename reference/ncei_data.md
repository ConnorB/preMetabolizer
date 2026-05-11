# Get data from the NCEI Data Service API

Retrieves weather and climate data from NOAA's National Centers for
Environmental Information (NCEI) using the Access Data Service API.

## Usage

``` r
ncei_data(
  dataset,
  stations,
  start_date,
  end_date,
  data_types = NULL,
  units = "metric",
  include_station_name = TRUE,
  include_station_location = FALSE
)
```

## Arguments

- dataset:

  Character string. The dataset to query. Common values include
  `"daily-summaries"` (GHCND), `"global-hourly"` (ISD),
  `"global-summary-of-the-month"`, and `"global-summary-of-the-year"`.
  New datasets are added periodically; use
  [`ncei_datasets()`](https://connorb.github.io/preMetabolizer/reference/ncei_datasets.md)
  to list what is currently available.

- stations:

  Character vector of station identifiers, such as those returned by
  [`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
  or
  [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md).

- start_date, end_date:

  Start and end of the requested period, as `Date` objects or
  `"YYYY-MM-DD"` strings.

- data_types:

  Optional character vector of data type codes to include (e.g.,
  `c("TMAX", "TMIN", "PRCP")` for daily summaries). When `NULL` all
  available types are returned.

- units:

  Character string. `"metric"` (default) or `"standard"`.

- include_station_name:

  Logical. When `TRUE` (default), a `NAME` column is included in the
  result.

- include_station_location:

  Logical. When `TRUE`, `LATITUDE`, `LONGITUDE`, and `ELEVATION` columns
  are added. Default `FALSE`.

## Value

A data frame with one row per observation. The `STATION` column
identifies the source station and `DATE` records the observation time.
Additional columns depend on the requested `dataset` and `data_types`.

## Details

This function calls `https://www.ncei.noaa.gov/access/services/data/v1`.
Data are returned in CSV format and read into a data frame with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).

The `"daily-summaries"` dataset returns one row per station per day with
columns such as `PRCP`, `TMAX`, and `TMIN`. The `"global-hourly"`
dataset returns ISD records in which several key variables (`TMP`,
`WND`, `DEW`, `SLP`) contain comma-separated sub-fields rather than
simple numeric values.

## See also

[`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
to search for station identifiers,
[`ncei_datasets()`](https://connorb.github.io/preMetabolizer/reference/ncei_datasets.md)
to list available datasets.

## Examples

``` r
if (FALSE) { # \dontrun{
# Daily temperature and precipitation at a single station
ncei_data(
  dataset = "daily-summaries",
  stations = "USW00023183",
  start_date = "2023-01-01",
  end_date = "2023-12-31",
  data_types = c("TMAX", "TMIN", "PRCP")
)

# Hourly ISD observations
ncei_data(
  dataset = "global-hourly",
  stations = "72469023183",
  start_date = "2023-06-01",
  end_date = "2023-06-30"
)
} # }
```
