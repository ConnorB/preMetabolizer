# Download NASA POWER hourly data

Downloads hourly meteorological data from the NASA POWER project for the
date range covered by a time-series data frame.

## Usage

``` r
get_nasa_data(
  data,
  datetime_col = "dateTime",
  site_col = NULL,
  lat = NULL,
  lon = NULL,
  elev_m = NULL,
  params = c("PSC", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR", "T2M"),
  max_attempts = 5
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

- lat, lon, elev_m:

  Single numeric values used for single-site data. If omitted, `data`
  may instead contain single-valued `lat`, `lon`, and `elev_m` columns.
  When `data` contains multiple sites, these values must be supplied as
  per-site columns in `data`.

- params:

  Case-insensitive character vector of solar, meteorological, or
  climatology parameters to download. Defaults to `"PSC"`,
  `"ALLSKY_SFC_SW_DWN"`, `"PRECTOTCORR"`, and `"T2M"`. See
  [get_power](https://docs.ropensci.org/nasapower/reference/get_power.html)
  for more information.

- max_attempts:

  Number of retry attempts for failed API calls. Default is 5.

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
