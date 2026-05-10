# Changelog

## preMetabolizer 0.0.0.9000

- Added `french_creek` dataset: 5-minute dissolved oxygen and water
  temperature records from French Creek, Laramie, WY (Aug–Sep 2012),
  courtesy of Bob Hall. Source: Hall et al. (2016), .
- Added “Preparing French Creek Data for Stream Metabolism Modeling”
  vignette demonstrating a complete preMetabolizer workflow: solar time
  conversion, modeled PAR, elevation-corrected barometric pressure, O2
  saturation, and streamMetabolizer input assembly.
- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  now uses the Benson and Krause umol/kg coefficients from Garcia and
  Gordon (1992), converts to mg/L with salinity-aware density, and
  applies the corrected vapor-pressure term for non-standard pressure
  (#TBD).
- [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md)
  with `method = "MIMSY"` now returns physically correct values. The
  Antoine equation constant `B` was incorrectly set to 140.264 (should
  be 1435.264), causing vapor pressures that were orders of magnitude
  too large.
- [`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)
  no longer accepts a `.drop_units` argument; it always returns a plain
  numeric vector (#TBD).
- [`calc_water_height()`](https://connorb.github.io/preMetabolizer/reference/calc_water_height.md)
  now gives a clear error when `type = "unvented"` is used without
  providing `atmo_kPa`, rather than silently returning `NA`.
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  now uses `latitude` and `longitude` arguments. The old `lat`, `long`,
  and `lon` aliases are deprecated (#TBD).
- [`convert_flow()`](https://connorb.github.io/preMetabolizer/reference/convert_flow.md)
  now requires an explicit `from` argument specifying the input unit; it
  always returns a plain numeric vector instead of a `units` object
  (#TBD).
- `convert_PAR_to_SW()` and `convert_SW_to_PAR()` are no longer
  re-exported from preMetabolizer. Call
  [`streamMetabolizer::convert_PAR_to_SW()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_PAR_to_SW.html)
  and
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html)
  directly (#TBD).
- [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  now requires an explicit `from` argument; it always returns a plain
  numeric vector. Unit-bearing objects are no longer accepted as input
  (#TBD).
- [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md)
  no longer accepts a `drop_units` argument; it always returns a plain
  numeric vector. Elevation inputs must be plain numeric (meters)
  (#TBD).
- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  no longer errors on single-column data frames due to `drop = TRUE`
  subsetting.
- [`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md)
  now supports Kansas Mesonet network requests with `network = "KSRE"`,
  `"BBW"`, or `"EBW"`, accepts `"all"` as a station selector, and
  correctly reports invalid variable names in the error message
  (previously reported them as “Invalid stations:” instead of “Invalid
  variables:”).
- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  now accepts time-series data directly, infers per-site download date
  ranges from the data, uses either single-site `latitude` and
  `longitude` arguments or per-site `latitude`, `longitude`, and
  `elev_m` columns, interpolates NASA values to the input timestamps,
  and returns `light.obs` by converting `ALLSKY_SFC_SW_DWN` with
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html).
  The old `lat` and `lon` aliases are deprecated (#TBD).
- [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md)
  can now retrieve elevation values from the USGS Elevation Point Query
  Service for one or more coordinate pairs (#TBD).
- `has_units()` has been removed. The `units` package is no longer a
  dependency (#TBD).
- [`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
  can now retrieve Kansas Mesonet fire weather data in FW13 format for
  one station and date range (#TBD).
- [`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
  can now retrieve the most recently ingested Kansas Mesonet timestamp
  for each station at a requested interval (#TBD).
