# Align logger data to even, rounded timestamps

Snaps logger data onto an evenly spaced, rounded time grid. The time
step is inferred from the data, the grid is aligned to round times (e.g.
a series of 10-minute readings starting at 20:59 yields 21:00, 21:10,
...), and values are linearly interpolated onto the grid timestamps.
Only the grid timestamps are returned; grid times that fall in data gaps
wider than `max_gap` are left `NA`.

## Usage

``` r
even_timesteps(
  logger_data,
  datetime_col = NULL,
  site_col = NULL,
  max_gap = NULL,
  loggerData = lifecycle::deprecated()
)
```

## Arguments

- logger_data:

  Data frame or tibble containing timestamped logger data.

- datetime_col:

  Optional character string naming the POSIXct datetime column. If
  `NULL` (default), the single POSIXct column in `logger_data` is
  detected automatically; when `logger_data` contains more than one
  POSIXct column, `datetime_col` must be supplied.

- site_col:

  Optional character string naming a site column. When supplied, each
  site is aligned independently.

- max_gap:

  Optional single number giving the maximum gap, in seconds, across
  which values are interpolated. A grid timestamp is filled only when
  the observations bracketing it are at most `max_gap` seconds apart;
  non-numeric columns use the nearest observation within `max_gap / 2`
  seconds. Defaults to 1.5 times the inferred time step, which bridges
  ordinary clock offsets while leaving missing readings as `NA`. Use
  `Inf` to interpolate across all gaps.

- loggerData:

  **\[deprecated\]** Use `logger_data` instead.

## Value

A data frame or tibble, matching the input class, containing only the
evenly spaced, rounded timestamps with values interpolated from the
original observations. Rows with missing timestamps or missing site
values are dropped with a warning before the grid is built. Sites with
fewer than two distinct non-missing timestamps are dropped because an
evenly spaced grid cannot be inferred.

## Details

The time step is inferred from the modal difference between the sorted
unique timestamps for each site. Differences are rounded to the nearest
whole second, and small clock offsets around minute-level intervals are
snapped to the nearest minute. The grid runs from the first timestamp
rounded to the nearest step through the last timestamp rounded to the
nearest step, so slightly early or late boundary observations are
retained. Numeric and POSIXct columns are linearly interpolated onto the
grid; other columns are filled from the nearest observation, with a
warning. Use this after removing obvious duplicate or invalid
timestamps.

## Examples

``` r
# Align slightly offset 10-minute readings to round timestamps.
logger <- data.frame(
  datetime = as.POSIXct(
    c(
      "2024-01-01 20:59:33",
      "2024-01-01 21:09:31",
      "2024-01-01 21:19:29",
      "2024-01-01 21:29:37"
    ),
    tz = "UTC"
  ),
  temperature = c(10.0, 12.0, 14.0, 16.0)
)

even_timesteps(logger)
#>              datetime temperature
#> 1 2024-01-01 21:00:00    10.09030
#> 2 2024-01-01 21:10:00    12.09699
#> 3 2024-01-01 21:20:00    14.10197
#> 4 2024-01-01 21:30:00    16.00000

# Numeric values are linearly interpolated onto the rounded grid.
irregular <- data.frame(
  datetime = as.POSIXct(
    c(
      "2024-01-01 00:01:15",
      "2024-01-01 00:11:06",
      "2024-01-01 00:21:43"
    ),
    tz = "UTC"
  ),
  temperature = c(10, 20, 30)
)

even_timesteps(irregular)
#>              datetime temperature
#> 1 2024-01-01 00:00:00    10.00000
#> 2 2024-01-01 00:10:00    18.88325
#> 3 2024-01-01 00:20:00    28.38305

# By default, values are not interpolated across a missing reading.
with_gap <- data.frame(
  datetime = as.POSIXct(
    c(
      "2024-01-01 00:00:00",
      "2024-01-01 00:10:00",
      "2024-01-01 00:30:00",
      "2024-01-01 00:40:00"
    ),
    tz = "UTC"
  ),
  temperature = c(10, 12, 16, 18)
)

even_timesteps(with_gap)
#>              datetime temperature
#> 1 2024-01-01 00:00:00          10
#> 2 2024-01-01 00:10:00          12
#> 3 2024-01-01 00:20:00          NA
#> 4 2024-01-01 00:30:00          16
#> 5 2024-01-01 00:40:00          18

# Increase max_gap to interpolate across the 20-minute gap.
even_timesteps(with_gap, max_gap = 20 * 60)
#>              datetime temperature
#> 1 2024-01-01 00:00:00          10
#> 2 2024-01-01 00:10:00          12
#> 3 2024-01-01 00:20:00          14
#> 4 2024-01-01 00:30:00          16
#> 5 2024-01-01 00:40:00          18

# Align each site independently when sites have different sampling
# intervals.
multiple_sites <- data.frame(
  site = c("A", "A", "A", "B", "B", "B"),
  datetime = as.POSIXct(
    c(
      "2024-01-01 00:01:00",
      "2024-01-01 00:11:00",
      "2024-01-01 00:21:00",
      "2024-01-01 00:02:00",
      "2024-01-01 00:32:00",
      "2024-01-01 01:02:00"
    ),
    tz = "UTC"
  ),
  temperature = c(10, 11, 12, 20, 21, 22)
)

even_timesteps(
  multiple_sites,
  datetime_col = "datetime",
  site_col = "site"
)
#>              datetime site temperature
#> 1 2024-01-01 00:00:00    A    10.00000
#> 2 2024-01-01 00:10:00    A    10.90000
#> 3 2024-01-01 00:20:00    A    11.90000
#> 4 2024-01-01 00:00:00    B    20.00000
#> 5 2024-01-01 00:30:00    B    20.93333
#> 6 2024-01-01 01:00:00    B    21.93333

# Supply datetime_col when the data contains multiple POSIXct columns.
multiple_datetimes <- data.frame(
  date_time_utc = as.POSIXct(
    c(
      "2024-01-01 00:01:00",
      "2024-01-01 00:11:00",
      "2024-01-01 00:21:00"
    ),
    tz = "UTC"
  ),
  date_time_central = as.POSIXct(
    c(
      "2024-01-02 08:00:00",
      "2024-01-02 08:01:00",
      "2024-01-02 08:02:00"
    ),
    tz = "US/Central"
  ),
  temperature = c(10, 11, 12)
)

even_timesteps(
  multiple_datetimes,
  datetime_col = "date_time_utc"
)
#>         date_time_utc   date_time_central temperature
#> 1 2024-01-01 00:00:00 2024-01-02 08:00:00        10.0
#> 2 2024-01-01 00:10:00 2024-01-02 08:00:54        10.9
#> 3 2024-01-01 00:20:00 2024-01-02 08:01:54        11.9

# Exact observations are retained.
exact_times <- data.frame(
  datetime = as.POSIXct(
    c(
      "2024-01-01 00:00:00",
      "2024-01-01 00:10:00",
      "2024-01-01 00:20:00"
    ),
    tz = "UTC"
  ),
  temperature = c(10, 99, 20)
)

even_timesteps(exact_times)
#>              datetime temperature
#> 1 2024-01-01 00:00:00          10
#> 2 2024-01-01 00:10:00          99
#> 3 2024-01-01 00:20:00          20
```
