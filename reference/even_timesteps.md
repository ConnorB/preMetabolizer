# Fill missing rows in an even time series

Builds a complete timestamp sequence for logger data and joins the
original observations onto it. Missing timestamps become explicit rows
with `NA` values in the measured columns.

## Usage

``` r
even_timesteps(loggerData, datetime_col = "DateTime_UTC", site_col = NULL)
```

## Arguments

- loggerData:

  Data frame or tibble containing timestamped logger data.

- datetime_col:

  Character string naming the POSIXct datetime column. Defaults to
  `"DateTime_UTC"`.

- site_col:

  Optional character string naming a site column. When supplied, each
  site is completed independently.

## Value

A data frame or tibble, matching the input class, with all original rows
plus inserted `NA` rows for missing time steps.

## Details

The time step is inferred from the sorted unique timestamps for each
site. Use this after removing obvious duplicate or invalid timestamps.

## Examples

``` r
df <- data.frame(
  DateTime_UTC = as.POSIXct(
    c("2024-01-01 00:00", "2024-01-01 01:00", "2024-01-01 03:00"),
    tz = "UTC"
  ),
  temp_C = c(10.1, 10.4, 10.5)
)

even_timesteps(df)
#>          DateTime_UTC temp_C
#> 1 2024-01-01 00:00:00   10.1
#> 2 2024-01-01 01:00:00   10.4
#> 3 2024-01-01 02:00:00     NA
#> 4 2024-01-01 03:00:00   10.5

df_multi <- data.frame(
  DateTime_UTC = c(
    as.POSIXct(
      c("2024-01-01 00:00", "2024-01-01 01:00", "2024-01-01 03:00"),
      tz = "UTC"
    ),
    as.POSIXct(c("2024-01-01 00:00", "2024-01-01 00:30"), tz = "UTC")
  ),
  Site = c("A", "A", "A", "B", "B")
)

even_timesteps(df_multi, site_col = "Site")
#>          DateTime_UTC Site
#> 1 2024-01-01 00:00:00    A
#> 2 2024-01-01 01:00:00    A
#> 3 2024-01-01 02:00:00    A
#> 4 2024-01-01 03:00:00    A
#> 5 2024-01-01 00:00:00    B
#> 6 2024-01-01 00:30:00    B
```
