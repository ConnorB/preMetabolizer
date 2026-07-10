# Get Kansas Mesonet time-series data

Retrieves weather observations for one or more Kansas Mesonet stations
or a supported Kansas Mesonet subnetwork.

## Usage

``` r
ks_meso_time_series(
  stations = NULL,
  network = NULL,
  start_date,
  end_date,
  interval,
  vars = NULL
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

  Start date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD`
  format.

- end_date:

  End date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD` format.

- interval:

  Data interval. Must be one of `"hour"`, `"5min"`, or `"day"`.

- vars:

  Character vector of variables to retrieve. Defaults to common weather
  variables.

## Value

A tibble containing requested observations with snake_case column names.
Kansas Mesonet timestamps are parsed as `POSIXct` values in the fixed
Mesonet time zone, `Etc/GMT+6`.

## Details

**\[experimental\]**

Large date ranges are split into chunks automatically to stay within the
Kansas Mesonet API record limit. Data are returned directly and are not
written to a local cache.

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
konza <- ks_meso_time_series(
  stations = "Konza Prairie",
  start_date = "2024-06-01",
  end_date = "2024-06-07",
  interval = "hour",
  vars = c("TEMP2MAVG", "RELHUM2MAVG", "PRESSUREAVG")
)
} # }
```
