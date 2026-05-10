# Downloading Kansas Mesonet data

## Introduction

The [Kansas Mesonet](https://mesonet.k-state.edu/) is a statewide
network of automated weather stations operated by Kansas State
University. It provides sub-hourly meteorological data — including air
temperature, relative humidity, precipitation, solar radiation, wind,
and soil measurements — for more than 60 stations across Kansas.

This vignette walks through the workflow for discovering stations,
browsing the variable catalog, downloading data, and reading cached
files using the preMetabolizer Mesonet functions. We use **Konza
Prairie**, a long-term ecological research (LTER) site in northeastern
Kansas, as our example station.

> **Note:** Functions that contact the Mesonet API require an internet
> connection. Code chunks marked with `#| eval: false` will not run
> during package installation. Run them interactively in your own
> session.

``` r

library(preMetabolizer)
library(dplyr)
library(ggplot2)
```

## Explore the variable catalog

[`ks_meso_vars()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_vars.md)
retrieves and parses the current Kansas Mesonet variable metadata,
including CSV headings, variable names, units, and descriptions.

``` r

mesonet_vars <- ks_meso_vars()
```

Search the catalog to find variables of interest:

``` r

mesonet_vars |>
  filter(grepl("temp|precip|solar|humidity", desc, ignore.case = TRUE)) |>
  select(var, tidy_name, units, desc)
```

## Discover stations

[`ks_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_stations.md)
retrieves metadata for all Mesonet stations, including location, network
affiliation, and whether the station supports FW13 fire weather reports.

``` r

stations <- ks_meso_stations()
glimpse(stations)
```

Filter to find Konza Prairie and confirm the exact station name used by
the API:

``` r

konza <- stations |>
  filter(grepl("Konza", StationName, ignore.case = TRUE))

konza
```

The station name recognized by the API is `"Konza Prairie"`.

## Check station activity

[`ks_meso_station_activity()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_station_activity.md)
shows the observation intervals available at each station and the date
range of archived data. This is useful for verifying that a station has
data for your study period and identifying the finest available temporal
resolution.

``` r

activity <- ks_meso_station_activity()

activity |>
  filter(station == "Konza Prairie")
```

## Check the most recent observation

[`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
returns the timestamp of the latest ingested observation for every
station at a given interval, which is helpful for monitoring live data
pipelines.

``` r

recent <- ks_meso_most_recent(interval = "hour")

recent |>
  filter(station == "Konza Prairie")
```

## Download data

[`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md)
downloads Mesonet data for one or more stations and saves the result to
a local cache directory. Large date ranges are split into chunks
automatically to stay within the API record limit.

Here we download hourly data for Konza Prairie for the 2024 calendar
year, requesting air temperature, relative humidity, precipitation, and
surface pressure.

``` r

download_result <- get_ks_meso(
  stations   = "Konza Prairie",
  start_date = "2024-01-01",
  end_date   = "2024-12-31",
  interval   = "hour",
  vars       = c("TEMP2MAVG", "RELHUM2MAVG", "PRECIP", "PRESSUREAVG")
)

str(download_result)
```

## Read cached data

Once downloaded,
[`read_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/read_ks_meso.md)
reads the cached file back into R. The `TIMESTAMP` column is
automatically parsed as a `POSIXct` in the Mesonet timezone
(`Etc/GMT+6`, i.e., CST without daylight saving).

``` r

konza_hourly <- read_ks_meso(
  station    = "Konza Prairie",
  start_date = "2024-01-01",
  end_date   = "2024-12-31",
  interval   = "hour"
)

glimpse(konza_hourly)
```

## Example workflow: monthly precipitation

With the data in hand, standard `dplyr` and `ggplot2` workflows apply.
The example below summarizes monthly precipitation totals and daily
temperature ranges.

``` r

monthly_precip <- konza_hourly |>
  mutate(month = lubridate::floor_date(TIMESTAMP, "month")) |>
  group_by(month) |>
  summarise(
    precip_mm    = sum(PRECIP, na.rm = TRUE),
    temp_mean_C  = mean(TEMP2MAVG, na.rm = TRUE),
    .groups      = "drop"
  )

ggplot(monthly_precip, aes(month, precip_mm)) +
  geom_col(fill = "#4575b4") +
  labs(
    x = NULL,
    y = "Precipitation (mm)",
    title = "Monthly precipitation — Konza Prairie 2024"
  ) +
  theme_bw()
```

## FW13 fire weather data

[`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
retrieves fire weather records in the standard FW13 format used by USDA
Forest Service fire behavior modeling tools.

``` r

fw13_records <- ks_meso_fw13(
  station    = "Konza Prairie",
  start_date = "2024-04-01",
  end_date   = "2024-04-30"
)

# Each element is one fixed-width FW13 record
head(fw13_records, 3)
```
