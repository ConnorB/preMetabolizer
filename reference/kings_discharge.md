# Kings Creek daily water data

Daily USGS water data for Kings Creek at monitoring location
`USGS-06879650`.

## Usage

``` r
kings_discharge
```

## Format

A tibble with 716 rows and 5 variables:

- monitoring_location_id:

  USGS monitoring location ID.

- time:

  Date of observation.

- value:

  Observed value. Units depend on `parameter_code`.

- parameter_code:

  USGS parameter code: `00060` = discharge, `00065` = gage height,
  `00010` = water temperature.

- qualifier:

  USGS data qualifier.

## Source

USGS Water Data API, retrieved with
`dataRetrieval::read_waterdata_daily()`.

## Details

The dataset includes daily observations for water year 2025, from
2024-10-01 through 2025-09-30.
