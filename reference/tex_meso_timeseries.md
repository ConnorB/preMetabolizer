# Get recent TexMesonet time-series data

Retrieves recent time-series observations for a single TWDB TexMesonet
station.

## Usage

``` r
tex_meso_timeseries(
  site_id,
  prior_minutes,
  variable = c("all", "temperature", "humidity", "barometric_pressure", "precip",
    "wind_speed")
)
```

## Arguments

- site_id:

  TexMesonet station ID.

- prior_minutes:

  Number of minutes before the current time to retrieve.

- variable:

  Data type to retrieve. Use `"all"` for the charting fields endpoint,
  or one of `"temperature"`, `"humidity"`, `"barometric_pressure"`,
  `"precip"`, or `"wind_speed"` for the single-variable endpoints.

## Value

A tibble of observations with UTC `date_time` values. Single-variable
requests return `value` and `date_time` columns and have `"field_name"`,
`"station_name"`, `"station_id"`, and `"units"` attributes. Requests
with `variable = "all"` return one column per charting field and have
`"station_name"` and `"station_id"` attributes.

## Details

**\[experimental\]**

TexMesonet's public time-series API retrieves recent observations by
station and look-back window. The API may return different fields by
station because not all stations measure every parameter.

## References

TexMesonet APIs: <https://www.texmesonet.org/Apis>

## Examples

``` r
if (FALSE) { # \dontrun{
tex_meso_timeseries(2, prior_minutes = 60)
tex_meso_timeseries(2, prior_minutes = 60, variable = "temperature")
} # }
```
