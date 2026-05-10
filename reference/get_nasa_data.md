# Download NASA POWER hourly data

Downloads hourly meteorological data from the NASA POWER project for the
date range covered by a time-series data frame and interpolates the
result to the input timestamps.

## Usage

``` r
get_nasa_data(
  data,
  datetime_col = "dateTime",
  site_col = NULL,
  latitude = NULL,
  longitude = NULL,
  elev_m = NULL,
  params = c("PSC", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR", "T2M"),
  max_attempts = 5,
  lat = lifecycle::deprecated(),
  lon = lifecycle::deprecated()
)
```

## Arguments

- data:

  A data frame or tibble containing time-series data.

- datetime_col:

  Character string specifying the date-time column in `data`. Defaults
  to `"dateTime"`.

- site_col:

  Optional character string specifying the site column in `data`. If
  `NULL`, `data` is treated as a single site.

- latitude, longitude:

  Single numeric values used for single-site data. If omitted, `data`
  may instead contain single-valued `latitude` and `longitude` columns.
  When `data` contains multiple sites, coordinates must be supplied as
  per-site columns in `data`.

- elev_m:

  Single numeric site elevation in meters. If omitted, `data` may
  instead contain a single-valued `elev_m` column. When `data` contains
  multiple sites, elevation must be supplied as a per-site column in
  `data`.

- params:

  Case-insensitive character vector of solar, meteorological, or
  climatology parameters to download. Defaults to `"PSC"`,
  `"ALLSKY_SFC_SW_DWN"`, `"PRECTOTCORR"`, and `"T2M"`. See
  [get_power](https://docs.ropensci.org/nasapower/reference/get_power.html)
  for more information.

- max_attempts:

  Number of retry attempts for failed API calls. Default is 5.

- lat, lon:

  **\[deprecated\]** Use `latitude` and `longitude` instead. Data
  columns named `lat` and `lon` are also deprecated; use `latitude` and
  `longitude` columns instead.

## Value

A tibble interpolated to the non-missing timestamps in `data`, with
columns:

- Site column:

  Site names in the column named by `site_col`, when supplied

- dateTime:

  timestamps from `data` in UTC

- PSC:

  Elevation-corrected barometric pressure (kPa)

- ALLSKY_SFC_SW_DWN:

  All Sky Surface Shortwave Downward Irradiance (W/m²), when requested

- light.obs:

  Observed photosynthetically active radiation (µmol/m²/s), converted
  from `ALLSKY_SFC_SW_DWN` when requested

- T2M:

  Average air temperature at 2 m above the surface (°C)

- PRECTOTCORR:

  MERRA-2 bias corrected total precipitation (mm/hr)

## Examples

``` r
if (FALSE) { # \dontrun{
stream_data <- data.frame(
  dateTime = as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
)

get_nasa_data(
  stream_data,
  latitude = 39.1,
  longitude = -96.6,
  elev_m = 320
)

site_data <- data.frame(
  site = c("a", "b"),
  dateTime = as.POSIXct(c(
    "2024-06-01 12:00:00",
    "2024-06-01 12:00:00"
  ), tz = "UTC"),
  latitude = c(39.1, 40.0),
  longitude = c(-96.6, -97.2),
  elev_m = c(320, 340)
)

get_nasa_data(site_data, site_col = "site")
} # }
```
