## data-raw/french_creek.R
## Produces data/french_creek.rda
##
## Source: Hall, R. O., Jr., Tank, J. L., Baker, M. A., Rosi-Marshall,
##   E. J., & Hotchkiss, E. R. (2016). Metabolism, gas exchange, and
##   carbon spiraling in rivers. Ecosystems, 19(1), 73-86.
##   doi: 10.1890/14-0631.1
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
