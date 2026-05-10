# preMetabolizer: Prepare data for stream metabolism modeling

preMetabolizer provides helpers for building the environmental time
series commonly needed before fitting stream metabolism models. It
includes tools to download external meteorological and elevation data,
align irregular time series, convert hydrology and atmospheric units,
calculate water and gas chemistry quantities, and assemble light and
pressure inputs for streamMetabolizer.

## Coordinates

Functions that require locations use `latitude` and `longitude` in
decimal degrees. Latitude ranges from -90 to 90, longitude ranges from
-180 to 180, and western longitudes are negative.

## Main workflows

- Find and download meteorological data with
  [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md),
  [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md),
  [`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md),
  [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md),
  and
  [`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md).

- Retrieve or calculate site context with
  [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md),
  [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md),
  [`convert_UTC_to_solartime()`](https://connorb.github.io/preMetabolizer/reference/convert_UTC_to_solartime.md),
  and
  [`calc_light()`](https://connorb.github.io/preMetabolizer/reference/calc_light.md).

- Prepare model inputs with
  [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md),
  [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md),
  [`calc_water_height()`](https://connorb.github.io/preMetabolizer/reference/calc_water_height.md),
  and the built-in example datasets.

- Summarize and check data with
  [`flag_z()`](https://connorb.github.io/preMetabolizer/reference/flag_z.md),
  [`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md),
  [`calc_cv()`](https://connorb.github.io/preMetabolizer/reference/calc_cv.md),
  and
  [`calc_bin_width()`](https://connorb.github.io/preMetabolizer/reference/calc_bin_width.md).

## Built-in datasets

[french_creek](https://connorb.github.io/preMetabolizer/reference/french_creek.md)
contains 5-minute dissolved oxygen and water temperature observations
for a complete metabolism-preparation example.
[kings_discharge](https://connorb.github.io/preMetabolizer/reference/kings_discharge.md)
contains daily USGS discharge, gage height, and water temperature
observations for flow-duration and seasonal summaries.

## See also

[`streamMetabolizer::metab()`](https://rdrr.io/pkg/streamMetabolizer/man/metab.html),
<https://connorb.github.io/preMetabolizer/>, and
<https://github.com/ConnorB/preMetabolizer/issues>.

## Author

**Maintainer**: Connor Brown <ConnorBrown1996@gmail.com>
([ORCID](https://orcid.org/0000-0002-9680-8930))

Authors:

- Connor Brown <ConnorBrown1996@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9680-8930))
