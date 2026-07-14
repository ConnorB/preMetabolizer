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
  [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md),
  [`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md),
  [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md),
  and
  [`ks_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_time_series.md).
  Retrieve mesonet station observations with
  [`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md),
  [`iem_current()`](https://connorb.github.io/preMetabolizer/reference/iem_current.md),
  [`iem_ob_history()`](https://connorb.github.io/preMetabolizer/reference/iem_ob_history.md),
  [`tex_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_stations.md),
  [`tex_meso_current()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_current.md),
  and
  [`tex_meso_time_series()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_time_series.md).

- Retrieve or calculate site context with
  [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md),
  [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md),
  [`convert_to_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md),
  and
  [`calc_par()`](https://connorb.github.io/preMetabolizer/reference/calc_par.md).

- Prepare model inputs with
  [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md),
  [`calc_o2_sat()`](https://connorb.github.io/preMetabolizer/reference/calc_o2_sat.md),
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

[`streamMetabolizer::metab()`](https://connorb.github.io/streamMetabolizer/reference/metab.html),
<https://connorb.github.io/preMetabolizer/>, and
<https://github.com/ConnorB/preMetabolizer/issues>.

## Author

**Maintainer**: Connor Brown <ConnorBrown1996@gmail.com>
([ORCID](https://orcid.org/0000-0002-9680-8930))

Authors:

- Connor Brown <ConnorBrown1996@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9680-8930))
