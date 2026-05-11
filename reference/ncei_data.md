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

  Logical. When `TRUE` (default), a `station_name` column is included in
  the result.

- include_station_location:

  Logical. When `TRUE`, `latitude`, `longitude`, and `elevation` columns
  are added. Default `FALSE`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per observation and snake_case column names. Leading
columns (when present) are `station_id`, `station_name`, `latitude`,
`longitude`, `elevation`, and either `datetime` (`POSIXct` UTC, for
sub-daily datasets) or `date` (`Date`, for daily and coarser datasets).
Remaining columns depend on the requested `dataset` and `data_types`.
Columns that are entirely `NA` are dropped.

## Details

This function calls `https://www.ncei.noaa.gov/access/services/data/v1`.
Data are returned in CSV format and parsed into a tibble.

For `dataset = "global-hourly"` (Integrated Surface Database), the
mandatory packed fields are expanded into typed numeric columns with SI
units and missing-value sentinels converted to `NA`:

- `WND`:

  `wind_direction` (degrees), `wind_direction_quality`,
  `wind_type_code`, `wind_speed` (m/s), `wind_speed_quality`.

- `CIG`:

  `ceiling_height` (m), `ceiling_quality`, `ceiling_determination_code`,
  `ceiling_cavok`.

- `VIS`:

  `visibility` (m), `visibility_quality`, `visibility_variability_code`,
  `visibility_variability_quality`.

- `TMP`:

  `temperature` (°C), `temperature_quality`.

- `DEW`:

  `dew_point_temperature` (°C), `dew_point_quality`.

- `SLP`:

  `sea_level_pressure` (hPa), `sea_level_pressure_quality`.

- `AA1`–`AA4`:

  `precipitation_period_hours` (hr), `precipitation` (mm),
  `precipitation_condition_code`, `precipitation_quality`.

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

# Hourly ISD observations with expanded mandatory fields
ncei_data(
  dataset = "global-hourly",
  stations = "72469023183",
  start_date = "2023-06-01",
  end_date = "2023-06-30"
)
} # }
```
