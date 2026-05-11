# Get GHCNh hourly observations

Downloads Global Historical Climatology Network hourly (GHCNh) data for
one or more stations and a date range, returning a parsed tibble ready
for use in stream metabolism modelling.

## Usage

``` r
get_ghcnh(stations, start_date, end_date, quiet = FALSE)
```

## Arguments

- stations:

  Character vector of GHCNh station identifiers (e.g., `"USW00023183"`).
  These match the `station_id` column returned by
  [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  and
  [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md).

- start_date, end_date:

  Start and end of the requested period as `Date` objects or
  `"YYYY-MM-DD"` strings.

- quiet:

  Logical. When `TRUE` progress messages are suppressed. Default
  `FALSE`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with one row per station-hour. Key columns include:

- `station_id`:

  Station identifier.

- `station_name`:

  Station name (when available).

- `datetime`:

  Observation time as a UTC `POSIXct`.

- `temperature`:

  Air temperature in degrees Celsius.

- `dew_point_temperature`:

  Dew-point temperature (°C).

- `relative_humidity`:

  Relative humidity (%).

- `wind_direction`:

  Wind direction (degrees clockwise from north).

- `wind_speed`:

  Wind speed (m/s).

- `sea_level_pressure`:

  Sea-level pressure (hPa).

- `precipitation`:

  Precipitation (mm).

Columns that are entirely `NA` across all stations and hours are dropped
from the result.

## Details

GHCNh v1.1.0 data are served as pipe-separated (PSV) files from the NOAA
NCEI archive, one file per station per year. Data are current through
the present year. The function downloads all station-year combinations
covering the requested date range in parallel.

Sentinel values of `-9999` are converted to `NA`. Columns that are all
`NA` are removed.

Multiple stations and years are downloaded in parallel using up to four
concurrent connections.

## See also

[`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md),
[`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch WY 2025 hourly data for a station near Konza Prairie
konza <- closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50
)
hourly <- get_ghcnh(
  stations = konza$station_id[1],
  start_date = "2024-10-01",
  end_date = "2025-09-30"
)
} # }
```
