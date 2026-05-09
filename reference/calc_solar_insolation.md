# Model solar insolation on a horizontal surface (W/m2 == J/s/m2) as in http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html

Adapted from `streamMetabolizer`, included under the CC0 1.0 Universal
public domain dedication:
https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits

## Usage

``` r
calc_solar_insolation(
  app.solar.time,
  latitude,
  max.insolation = streamMetabolizer::convert_PAR_to_SW(2326),
  format = c("degrees", "radians")
)
```

## Arguments

- app.solar.time:

  POSIXct vector of date-time values in apparent solar time, e.g., as
  returned by
  `convert_UTC_to_solartime(..., time.type="apparent solar")`

- latitude:

  numeric. Site latitude in decimal degrees between -90 and 90.

- max.insolation:

  insolation rate at solar noon, W/m2 == J/s/m2. varies greatly with
  atmospheric conditions

- format:

  character. `"degrees"` or `"radians"`.
