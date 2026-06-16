# Changelog

## preMetabolizer 0.0.0.9000

- [`cdo_data()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  [`cdo_datasets()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  [`cdo_datacategories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  [`cdo_datatypes()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  [`cdo_locationcategories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  [`cdo_locations()`](https://connorb.github.io/preMetabolizer/reference/cdo.md),
  and
  [`cdo_stations()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  wrap the seven endpoints of the NCEI Climate Data Online (CDO) Web
  Services v2 API; they auto-paginate, parse date and numeric columns,
  read the API token from the `API_NCEI_CDO` environment variable,
  throttle to the per-token limit of 5 requests per second, and abort
  once the 10,000 requests-per-day limit is reached for the session (use
  [`cdo_request_count()`](https://connorb.github.io/preMetabolizer/reference/cdo_request_count.md)
  to inspect and
  [`cdo_reset_request_count()`](https://connorb.github.io/preMetabolizer/reference/cdo_request_count.md)
  to clear the session counter) (no issue).

- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  now queries the NCEI Search API with a bounding box instead of
  downloading the full MSHR station archive; the `state` and `clean`
  arguments have been removed (no issue).

- `download_ghcnh()` and `read_ghcnh()` have been replaced by
  [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md),
  which downloads GHCNh files and returns a parsed tibble directly (no
  issue).

- [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)
  now uses the GHCNh v1.1.0 archive with per-year PSV files; it can
  retrieve data for multiple stations in parallel, accepts a date range
  instead of individual years, and returns column names in `snake_case`
  (no issue).

- [`get_noaa_ghg()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_ghg.md)
  downloads the NOAA Global Monitoring Laboratory globally averaged
  monthly mean records for CO2, CH4, N2O, and SF6 and returns them
  stacked long with a `gas` label, a mid-month `date`, and a `unit`
  column; each gas is reported in its standard NOAA unit by default (CO2
  in ppm, CH4 and N2O in ppb, SF6 in ppt), or pass `units` (`"ppm"`,
  `"ppb"`, or `"ppt"`) to convert every gas to a common unit; `-9.99`
  uncertainty sentinels are read as `NA` (no issue).

- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  now queries the NCEI Search API for GHCND stations instead of parsing
  the MSHR fixed-width archive; it accepts `bbox`, `start_date`,
  `end_date`, `data_types`, and `text` arguments; the `state`, `clean`,
  and `debug` arguments have been removed (no issue).

- [`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md)
  computes a bounding box from a centre latitude, longitude, and radius
  in kilometres (no issue).

- [`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
  can retrieve data from any NCEI dataset (e.g., `"daily-summaries"`,
  `"global-hourly"`) via the NCEI Access Data Service API (no issue).

- [`ncei_datasets()`](https://connorb.github.io/preMetabolizer/reference/ncei_datasets.md)
  retrieves metadata about an NCEI dataset from the Support Service API
  (no issue).

- [`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
  searches for stations across any NCEI dataset via the NCEI Search
  Service API, with optional bounding box, date range, and data type
  filters (no issue).

- Added `french_creek` dataset: 5-minute dissolved oxygen and water
  temperature records from French Creek, Laramie, WY (Aug–Sep 2012),
  courtesy of Bob Hall. Source: Hall et al. (2016), .

- Added “Preparing French Creek Data for Stream Metabolism Modeling”
  vignette demonstrating a complete preMetabolizer workflow: solar time
  conversion, modeled PAR, elevation-corrected barometric pressure, O2
  saturation, and streamMetabolizer input assembly.

- Data-download helpers now throttle HTTP requests and use bounded
  parallel request execution to avoid sending too many simultaneous
  requests to remote services (no issue).

- Mesonet helpers now return snake_case table columns and use consistent
  station and network identifiers such as `station_id`, `station_name`,
  `network`, and `network_name` across Kansas Mesonet, TexMesonet, and
  IEM results (no issue).

- Messages, warnings, and errors now consistently use cli formatting
  across the package (no issue).

- [`calc_Arsat()`](https://connorb.github.io/preMetabolizer/reference/calc_Arsat.md)
  calculates dissolved argon saturation (the concentration in
  equilibrium with the atmosphere) from water temperature, barometric
  pressure, and salinity using the Hamme and Emerson (2004) solubility
  fit, returning umol/L or mg/L (no issue).

- [`calc_bin_width()`](https://connorb.github.io/preMetabolizer/reference/calc_bin_width.md)
  with `method = "doane"` now uses a self-consistent population skewness
  estimator; the previous formula mixed a population third moment with
  the sample standard deviation, biasing the bin width for small samples
  (no issue).

- [`calc_CH4sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CH4sat.md)
  calculates dissolved methane saturation (the concentration in
  equilibrium with the atmosphere) from water temperature, barometric
  pressure, and salinity using Wiesenburg and Guinasso (1979), returning
  umol/L or mg/L (no issue).

- [`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
  and
  [`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md)
  now follow the standard Henry’s law form `[CO2*] = K0 * pCO2`. A
  spurious additional factor of total pressure (atmospheric +
  hydrostatic) previously under-reported dissolved CO2 at any site away
  from 1 atm; the error reached ~15% at typical high-elevation streams
  (no issue).

- [`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
  and
  [`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md)
  rename the `waterDepth_m` argument to `water_depth_m`; `waterDepth_m`
  is soft-deprecated (no issue).

- [`calc_CO2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2sat.md)
  calculates dissolved carbon dioxide saturation (the concentration in
  equilibrium with the atmosphere) using the Weiss and Price (1980)
  trace-gas solubility function, returning umol/L or mg/L (no issue).

- [`calc_K0()`](https://connorb.github.io/preMetabolizer/reference/calc_K0.md)
  renames the `waterDepth_m` argument to `water_depth_m`; `waterDepth_m`
  is soft-deprecated (no issue).

- [`calc_N2Osat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2Osat.md)
  calculates dissolved nitrous oxide saturation (the concentration in
  equilibrium with the atmosphere) using the Weiss and Price (1980)
  trace-gas solubility function, returning umol/L or mg/L (no issue).

- [`calc_N2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2sat.md)
  calculates dissolved nitrogen (N2) saturation (the concentration in
  equilibrium with the atmosphere) from water temperature, barometric
  pressure, and salinity using the Hamme and Emerson (2004) solubility
  fit, returning umol/L or mg/L (no issue).

- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  gains an `out_units` argument to return saturation in umol/L in
  addition to mg/L (no issue).

- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  now uses the Benson and Krause umol/kg coefficients from Garcia and
  Gordon (1992), converts to mg/L with salinity-aware density, and
  applies the corrected vapor-pressure term for non-standard pressure
  (no issue).

- [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md)
  with `method = "MIMSY"` now returns physically correct values. The
  Antoine equation constant `B` was incorrectly set to 140.264 (should
  be 1435.264), causing vapor pressures that were orders of magnitude
  too large.

- [`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)
  gains a `salinity` argument: freshwater (`salinity = 0`) uses the
  Tanaka et al. (2001) recommended pure-water (SMOW) equation up to 40°C
  and Kell (1975) above 40°C (to 150°C), and any nonzero salinity uses
  the Millero and Poisson (1981) one-atmosphere seawater equation of
  state (valid 0-40°C and salinity 0.5-43), warning when temperature or
  salinity falls outside the valid range of the equation used. The
  gas-saturation functions now share this single density helper instead
  of a private copy (no issue).

- [`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)
  no longer accepts a `.drop_units` argument; it always returns a plain
  numeric vector (no issue).

- [`calc_water_height()`](https://connorb.github.io/preMetabolizer/reference/calc_water_height.md)
  now gives a clear error when `type = "unvented"` is used without
  providing `atmo_kPa`, rather than silently returning `NA`.

- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  no longer accepts the deprecated `lat`, `long`, or `lon` arguments;
  use `latitude` and `longitude`. The returned tibble uses
  `station_name` and no longer includes always-`NA` `elevation` or
  `data_coverage` columns (no issue).

- [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)
  internals now share datetime parsing with the mesonet helpers and use
  the shared `check_*` input validators (no issue).

- [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)
  no longer errors with `object 'sid' not found` when called with
  `quiet = FALSE` (no issue).

- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  returns a tibble with `station_name` and no longer includes
  always-`NA` `elevation` or `data_coverage` columns (no issue).

- [`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
  returns a snake_case tibble led by `station_id`, `station_name`, and a
  parsed `datetime` (or `date`) column; for `dataset = "global-hourly"`
  the ISD mandatory fields (`WND`, `CIG`, `VIS`, `TMP`, `DEW`, `SLP`,
  `AA1`–`AA4`) are split into typed numeric columns with units applied
  and sentinels converted to `NA` (no issue).

- [`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
  returns a tibble with `station_name`; the always-`NA` `elevation` and
  `data_coverage` columns are removed (no issue).

- [`convert_flow()`](https://connorb.github.io/preMetabolizer/reference/convert_flow.md)
  now requires an explicit `from` argument specifying the input unit; it
  always returns a plain numeric vector instead of a `units` object (no
  issue).

- `convert_PAR_to_SW()` and `convert_SW_to_PAR()` are no longer
  re-exported from preMetabolizer. Call
  [`streamMetabolizer::convert_PAR_to_SW()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_PAR_to_SW.html)
  and
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html)
  directly (no issue).

- [`convert_to_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  and
  [`convert_from_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  replace `convert_UTC_to_solartime()` and `convert_solartime_to_UTC()`.
  The new functions use the standard 15 deg/hour longitude offset for
  mean solar time and delegate to
  [`SunCalcMeeus::solar_time()`](https://docs.r4photobiology.info/SunCalcMeeus/reference/solar_time.html)
  for apparent solar time. They are renamed to avoid shadowing the
  originals in `streamMetabolizer`, which can still be called directly
  when needed (no issue).

- [`convert_to_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  renames the `dateTime` argument to `date_time`; `dateTime` is
  soft-deprecated (no issue).

- [`convert_to_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  and
  [`convert_from_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  now accept a vector `longitude` (one site per timestamp), so apparent
  solar time can be computed for several sites in a single call rather
  than erroring inside `SunCalcMeeus` (no issue).

- `calc_light()` now computes the solar zenith angle via
  [`SunCalcMeeus::sun_zenith_angle()`](https://docs.r4photobiology.info/SunCalcMeeus/reference/sun_angles.html)
  (full Meeus algorithms) instead of an internal first-order declination
  approximation. The public signature is unchanged; PAR values shift by
  less than ~0.03 percent. The internal helpers
  `calc_solar_insolation()`, `calc_declination_angle()`,
  `calc_hour_angle()`, `calc_zenith_angle()`, `to_radians()`, and
  `to_degrees()` have been removed (no issue).

- `calc_light()` no longer routes PAR through
  [`streamMetabolizer::convert_PAR_to_SW()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_PAR_to_SW.html)
  and
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html).
  Those wrappers ultimately call
  [`LakeMetabolizer::par.to.sw.base()`](https://rdrr.io/pkg/LakeMetabolizer/man/par.to.sw.html)
  and `sw.to.par.base()`, which are constant-factor multiplications by
  0.473 and 2.114 respectively; because the factors are reciprocals the
  round-trip cancels exactly, so the conversion has been inlined as
  `max.PAR * cos(zenith)` (no issue).

- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  inlines the SW-to-PAR conversion (`SW * 2.114`, Britton and Dodd 1976)
  instead of calling
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html).
  Output values are unchanged (no issue).

- `streamMetabolizer` is no longer a hard dependency of preMetabolizer.
  It has been removed from `Imports:` and `Remotes:`, and install it
  separately if you intend to fit metabolism models on the prepared
  data. Its `convert_UTC_to_solartime()` and
  `convert_solartime_to_UTC()` are still callable via the
  `streamMetabolizer::` prefix (no issue).

- [`calc_par()`](https://connorb.github.io/preMetabolizer/reference/calc_par.md)
  replaces `calc_light()` to avoid shadowing
  [`streamMetabolizer::calc_light()`](https://rdrr.io/pkg/streamMetabolizer/man/calc_light.html).
  The function body and behaviour are unchanged; parameters `solar.time`
  and `max.PAR` were renamed to `solar_time` and `max_par` to use the
  package’s underscore convention (no issue).

- [`calc_par()`](https://connorb.github.io/preMetabolizer/reference/calc_par.md)
  now accepts vector `latitude` and `longitude` (one site per timestamp)
  so several sites can be modeled in a single call, and errors clearly
  when a coordinate length is neither 1 nor the number of timestamps (no
  issue).

- [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  now requires an explicit `from` argument; it always returns a plain
  numeric vector. Unit-bearing objects are no longer accepted as input.
  [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  is now the sole public pressure conversion helper;
  `convert_pressure_to_atm()` has been removed (no issue).

- [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md)
  no longer accepts a `drop_units` argument; it always returns a plain
  numeric vector. Elevation inputs must be plain numeric (meters) (no
  issue).

- `download_ghcnh()` now validates inputs more clearly, treats existing
  local files as skipped downloads, and reports skipped files in its
  summary (no issue).

- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  no longer errors on single-column data frames due to `drop = TRUE`
  subsetting.

- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  now infers the time step from the modal diff of timestamps rather than
  the first diff, so a single near-duplicate timestamp at the start of a
  series no longer causes a wrong (often much smaller) step to be
  inferred (no issue).

- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  renames the `loggerData` argument to `logger_data`; `loggerData` is
  soft-deprecated (no issue).

- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  now detects the POSIXct datetime column automatically; `datetime_col`
  defaults to `NULL` and only needs to be supplied when `logger_data`
  contains multiple POSIXct columns (no issue).

- [`flag_z()`](https://connorb.github.io/preMetabolizer/reference/flag_z.md)
  now uses the canonical Tukey biweight midvariance (Mosteller & Tukey
  1977; Lax 1985) with tuning constant `c = 9` for the robust scale
  estimate, instead of a non-canonical MAD-normalized weighted standard
  deviation. The returned z-scores are now comparable to a Gaussian z
  for normally distributed data (no issue).

- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  now accepts time-series data directly, infers per-site download date
  ranges from the data, uses either single-site `latitude` and
  `longitude` arguments or per-site `latitude`, `longitude`, and
  `elev_m` columns, interpolates NASA values to the input timestamps,
  returns `light.obs` by converting `ALLSKY_SFC_SW_DWN` with
  [`streamMetabolizer::convert_SW_to_PAR()`](https://rdrr.io/pkg/streamMetabolizer/man/convert_SW_to_PAR.html),
  and runs quietly by default. The old `lat` and `lon` aliases are
  deprecated (no issue).

- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  now detects the date-time column automatically; `datetime_col`
  defaults to `NULL` and only needs to be supplied when `data` contains
  multiple POSIXct or Date columns (no issue).

- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  now filters cached raw station metadata by `state`, validates options
  more clearly, and uses cached station metadata when remote
  modification times are unavailable (no issue).

- [`get_season()`](https://connorb.github.io/preMetabolizer/reference/get_season.md)
  now uses the per-year astronomical equinox and solstice dates
  (computed via Meeus, Astronomical Algorithms, 1991, chapter 26)
  instead of fixed month-day breakpoints, gains `hemisphere` and
  `labels` arguments, returns an ordered factor with levels Spring \<
  Summer \< Autumn \< Winter, and uses “Autumn” instead of “Fall” as the
  default label (no issue).

- [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md)
  can now retrieve elevation values from the USGS Elevation Point Query
  Service for one or more coordinate pairs (no issue).

- `has_units()` has been removed. The `units` package is no longer a
  dependency (no issue).

- [`iem_current()`](https://connorb.github.io/preMetabolizer/reference/iem_current.md),
  [`iem_daily()`](https://connorb.github.io/preMetabolizer/reference/iem_daily.md),
  [`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md),
  [`iem_obhistory()`](https://connorb.github.io/preMetabolizer/reference/iem_obhistory.md),
  [`iem_station()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md),
  and
  [`iem_stations()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
  can now retrieve Iowa Environmental Mesonet network metadata, station
  metadata, current observations, one-day observation histories, and
  daily summaries (no issue).

- [`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
  can now retrieve Kansas Mesonet fire weather data in FW13 format for
  one station and date range (no issue).

- [`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
  can now retrieve the most recently ingested Kansas Mesonet timestamp
  for each station at a requested interval (no issue).

- [`ks_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_timeseries.md)
  can now retrieve Kansas Mesonet station or network observations
  directly without writing to or reading from a local cache. The old
  `get_ks_meso()` and `read_ks_meso()` cache-oriented helpers have been
  removed before production release (no issue).

- [`rcpp_calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/rcpp_calc_exceedance_prob.md)
  now provides a C++ implementation of flow exceedance probability
  calculations (no issue).

- `read_ghcnh()` is now exported, validates file inputs, can suppress
  progress messages with `quiet = TRUE`, and handles files that lack
  `Station_name` or `Station_ID` columns (no issue).

- [`tex_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_stations.md),
  [`tex_meso_current()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_current.md),
  and
  [`tex_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_timeseries.md)
  can now retrieve Texas Water Development Board station metadata,
  current observations, and recent station time-series data from
  TexMesonet (no issue).
