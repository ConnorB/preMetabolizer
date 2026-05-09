## data-raw/french_creek.R
## Produces data/french_creek.rda
##
## Source: Hotchkiss, E. R., & Hall, R. O., Jr. (2015). Whole-stream
##   13C tracer addition reveals distinct fates of newly fixed carbon.
##   Ecology, 96, 403-416. https://doi.org/10.1890/14-0631.1
##
## Data courtesy of Bob Hall.
## Raw file: data-raw/french.csv (not committed; raw data from published paper)
## Latitude:  41.33 N
## Longitude: -106.3 W
## Depth:     0.16 m
## Timezone:  MDT (UTC-6); using "America/Denver" for correct DST handling

library(readr)
library(dplyr)
library(lubridate)

raw <- read_csv(
  "data-raw/french.csv",
  col_types = cols(
    station = col_character(),
    siteno = col_integer(),
    sonde = col_character(),
    date = col_character(),
    time = col_character(),
    temp = col_double(),
    oxy = col_double()
  )
)

french_creek <- raw |>
  mutate(
    datetime = paste(date, time) |>
      as.POSIXct(format = "%m/%d/%Y %H:%M:%S", tz = "America/Denver") |>
      with_tz("UTC")
  ) |>
  select(
    datetime,
    sonde,
    temp_C = temp,
    DO_mgL = oxy
  ) |>
  arrange(datetime)

usethis::use_data(french_creek, overwrite = TRUE)
