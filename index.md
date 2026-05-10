# preMetabolizer

`preMetabolizer` helps turn field and public environmental data into the
regular, physically consistent inputs needed for stream metabolism
models. It is designed for the work that usually happens before
[`streamMetabolizer`](https://github.com/ConnorB/streamMetabolizer):
finding nearby weather stations, downloading meteorological and
elevation data, standardizing timestamps, converting units, and
calculating light, pressure, dissolved gas, and water property
variables.

The package is most useful when you need to:

- prepare dissolved oxygen, water temperature, depth, light, and
  pressure inputs for metabolism models;
- fill timestamp gaps so logger data have even time steps;
- convert common hydrology and atmospheric units without adding a units
  dependency to your workflow;
- retrieve NOAA, NASA POWER, USGS elevation, or Kansas Mesonet data for
  a study site;
- make quick quality-control checks such as moving-window outlier flags,
  flow-duration curves, and seasonal summaries.

## Installation

`preMetabolizer` is not on CRAN. Install the development version from
GitHub:

``` r

# install.packages("pak")
pak::pak("ConnorB/preMetabolizer")
```

Or with `remotes`:

``` r

# install.packages("remotes")
remotes::install_github("ConnorB/preMetabolizer")
```

The package requires R (\>= 4.1.0). A companion fork of
`streamMetabolizer` is listed under `Remotes:` and will be installed
automatically.

## Quick start

Most workflows begin with logger data that contain a timestamp, observed
dissolved oxygen, and water temperature. The example below uses the
built-in French Creek dataset and builds the core columns expected by
[`streamMetabolizer::metab()`](https://rdrr.io/pkg/streamMetabolizer/man/metab.html).

``` r

library(preMetabolizer)
library(dplyr)

data(french_creek)

site_latitude <- 41.329678
site_longitude <- -106.359058
site_elev_m <- 3205

metab_input <- french_creek |>
  filter(sonde == "TOWN") |>
  select(datetime, DO_mgL, temp_C) |>
  even_timesteps(datetime_col = "datetime") |>
  mutate(
    solar.time = convert_UTC_to_solartime(
      datetime,
      longitude = site_longitude
    ),
    light = calc_light(
      solar.time,
      latitude = site_latitude,
      longitude = site_longitude
    ),
    bp_kPa = correct_bp(
      station_bp = 101.325,
      air_temp = temp_C,
      station_elev = 0,
      site_elev = site_elev_m
    ),
    DO.sat = calc_O2sat(temp_C, bp_kPa, units = "kPa")
  ) |>
  transmute(
    solar.time,
    DO.obs = DO_mgL,
    DO.sat,
    depth = 0.16,
    temp.water = temp_C,
    light
  )

head(metab_input)
```

For a more complete version of this workflow, including observed light
from NASA POWER, see
[`vignette("french-creek", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/french-creek.md).

## Common tasks

### Physical and chemical calculations

``` r

library(preMetabolizer)

# Dissolved oxygen saturation at 15°C, standard atmosphere, freshwater
calc_O2sat(temp_water = 15, atmo_press = 1, units = "atm")
#> [1] 10.07807

# Water density (kg/m³) at several temperatures
calc_water_density(water_temp = c(0, 10, 20, 30))
#> [1] 999.8395 999.6996 998.2041 995.6473

# Barometric pressure correction from a station at 300 m to a site at 500 m
correct_bp(station_bp = 101.3, air_temp = 15, station_elev = 300, site_elev = 500)
#> [1] 98.92626

# Dissolved CO2 concentration from xCO2, pressure, and water depth
calc_CO2_mgL(
  CO2_ppm = 420,
  temp_water = 20,
  waterDepth_m = 0.5,
  atmo_press = 101.325,
  press_units = "kPa"
)
```

### Unit conversion

``` r

# Stream discharge: cfs → cms
convert_flow(14.32, from = "cfs", to = "cms")
#> [1] 0.4054972

# Barometric pressure: kPa → atm
convert_pressure(101.325, from = "kPa", to = "atm")
#> [1] 1
```

### Solar radiation and light

``` r

# Convert a UTC timestamp to mean solar time at a site in Kansas
utc <- as.POSIXct("2024-06-21 18:00:00", tz = "UTC")
solar <- convert_UTC_to_solartime(utc, longitude = -96.6, time.type = "mean solar")

# Model PAR near local solar noon on the summer solstice
noon_solar <- convert_UTC_to_solartime(
  as.POSIXct("2024-06-21 18:00:00", tz = "UTC"),
  longitude = -96.6
)
calc_light(noon_solar, latitude = 39.1, longitude = -96.6)
#> [1] 2228.291
```

### Time series utilities

``` r

# Flag outliers with a moving-window robust Z-score
x <- c(1.0, 1.2, 1.1, 1.3, 50.0, 1.1, 1.2, 1.0)
flag_z(x, width = 5, threshold = 3)
#> [1] NA  NA  NA  NA  "Z" NA  NA  NA

# Determine meteorological season from a date
get_season(c("2024-01-15", "2024-04-15", "2024-07-04", "2024-10-15"))
#> [1] "Winter" "Spring" "Summer" "Fall"
```

### Public data access

Network functions download or query public data services and are best
run interactively. Results are cached where possible.

``` r

# Find NOAA stations near a site
closest_noaa_stations(
  latitude = 39.1068806,
  longitude = -96.6117151,
  dist_km = 50,
  state = "KS"
)

# Add NASA POWER pressure, radiation, precipitation, and air temperature
site_data <- data.frame(
  dateTime = as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
)

get_nasa_data(
  site_data,
  latitude = 39.1,
  longitude = -96.6,
  elev_m = 320
)
```

## Learn more

- [`vignette("french-creek", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/french-creek.md)
  prepares logger data for a metabolism model.
- [`vignette("kings-creek", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/kings-creek.md)
  analyzes daily stream discharge, flow duration, and seasonal
  variability.
- [`vignette("noaa", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/noaa.md)
  finds candidate NOAA stations.
- [`vignette("ghcnh", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/ghcnh.md)
  downloads and reads NOAA hourly GHCNh files.
- [`vignette("ks-mesonet", package = "preMetabolizer")`](https://connorb.github.io/preMetabolizer/articles/ks-mesonet.md)
  works with Kansas Mesonet station metadata and cached observations.

## License

GPL (\>= 3). See
[LICENSE.md](https://connorb.github.io/preMetabolizer/LICENSE.md).

## Issues

Bug reports and feature requests are welcome at
<https://github.com/ConnorB/preMetabolizer/issues>.
