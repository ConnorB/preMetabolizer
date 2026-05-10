# Downloading GHCNh data

## Introduction

The Global Historical Climatology Network hourly dataset, usually
abbreviated GHCNh, provides station-based hourly meteorological
observations from NOAA. preMetabolizer separates the workflow into two
steps:

- [`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md)
  downloads compressed CSV files for one station and one or more years.
- [`read_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/read_ghcnh.md)
  reads downloaded files, standardizes timestamps and common variable
  types, combines files, and removes columns that are entirely missing.

> **Note:** Downloading data requires an internet connection. The
> download chunks in this vignette use `#| eval: false` so they do not
> run during package installation.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Choose a station

GHCNh downloads require a station ID. If you do not already have one,
use the NOAA station helpers to find candidate stations near your study
site.

``` r

konza_noaa <- closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50,
  state = "KS"
)

station_id <- konza_noaa |>
  arrange(distance_km) |>
  pull(GHCND_ID) |>
  first()

station_id
```

See
[`vignette("noaa", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/noaa.md)
for more detail on station metadata and distance searches.

## Download files

[`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md)
downloads one compressed CSV file for each requested year. By default,
files are saved to preMetabolizer’s NOAA cache directory. You can also
provide an `output_dir` when you want the files to live with a project.

``` r

download_result <- download_ghcnh(
  station_id = station_id,
  years = 2022:2024
)

str(download_result)
```

For reproducible projects, it is often clearer to use a project
directory.

``` r

download_result <- download_ghcnh(
  station_id = station_id,
  years = 2022:2024,
  output_dir = "data/ghcnh",
  quiet = TRUE
)

download_result
```

If a requested file already exists locally,
[`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md)
skips it and reports it in `skipped_downloads`.

## Read downloaded files

Use
[`read_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/read_ghcnh.md)
with either a directory or an explicit vector of files. When
`combine = TRUE`, the default, all successfully read files are combined
into one data frame.

``` r

hourly <- read_ghcnh(directory = "data/ghcnh", quiet = TRUE)

hourly$files_read
hourly$date_range
hourly$stations

glimpse(hourly$data)
```

The returned object contains both the data and a small read summary. The
combined data are in `hourly$data`.

## Read a small local example

The example below creates a tiny CSV with the same shape as a GHCNh file
and reads it with
[`read_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/read_ghcnh.md).
It illustrates the standardization that happens after files are read:

- `DATE` is parsed into a UTC `DateTime` column.
- Temperature-like variables stored as tenths of degrees are divided by
  10.
- `-9999` sentinel values are converted to `NA`.
- Columns that are entirely missing are removed from the combined
  result.

``` r

example_dir <- file.path(tempdir(), "ghcnh-example")
dir.create(example_dir, showWarnings = FALSE)

example_file <- file.path(example_dir, "GHCNh_USW00000001_2024.csv")

readr::write_csv(
  tibble::tibble(
    Station_name = c("Example station", "Example station"),
    Station_ID = c("USW00000001", "USW00000001"),
    DATE = c("2024-06-01T00:00:00", "2024-06-01T01:00:00"),
    temperature = c(215, 209),
    dew_point_temperature = c(178, -9999),
    relative_humidity = c(78, 81),
    empty_column = c(NA, NA)
  ),
  example_file
)

example_hourly <- read_ghcnh(directory = example_dir, quiet = TRUE)

example_hourly$data
#> # A tibble: 2 × 6
#>   Station_name  DateTime            Station_ID temperature dew_point_temperature
#>   <chr>         <dttm>              <chr>            <dbl>                 <dbl>
#> 1 Example stat… 2024-06-01 00:00:00 USW000000…        21.5                  17.8
#> 2 Example stat… 2024-06-01 01:00:00 USW000000…        20.9                  NA  
#> # ℹ 1 more variable: relative_humidity <dbl>
```

## Summarize meteorological observations

Once the files are read, the result is a regular data frame. This
example summarizes daily mean air temperature and relative humidity from
the local example data.

``` r

daily <- example_hourly$data |>
  mutate(date = as.Date(DateTime)) |>
  group_by(date) |>
  summarise(
    temperature_C = mean(temperature, na.rm = TRUE),
    relative_humidity = mean(relative_humidity, na.rm = TRUE),
    .groups = "drop"
  )

daily
#> # A tibble: 1 × 3
#>   date       temperature_C relative_humidity
#>   <date>             <dbl>             <dbl>
#> 1 2024-06-01          21.2              79.5
```

For real downloaded data, the same pattern can be used to prepare
meteorology inputs, check station coverage, or compare candidate
stations before fitting stream metabolism models.

``` r

daily_temperature <- hourly$data |>
  mutate(date = as.Date(DateTime)) |>
  group_by(date) |>
  summarise(
    temperature_C = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(daily_temperature, aes(date, temperature_C)) +
  geom_line(color = "#2c7fb8") +
  labs(
    x = NULL,
    y = "Mean temperature (degrees C)",
    title = "Daily mean air temperature"
  ) +
  theme_bw()
```
