# Package index

## Package overview

- [`preMetabolizer`](https://connorb.github.io/preMetabolizer/reference/preMetabolizer-package.md)
  [`preMetabolizer-package`](https://connorb.github.io/preMetabolizer/reference/preMetabolizer-package.md)
  : preMetabolizer: Prepare data for stream metabolism modeling

## Data access

Download and read meteorological and hydrological data from external
sources.

- [`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
  : Get data from the NCEI Data Service API
- [`ncei_datasets()`](https://connorb.github.io/preMetabolizer/reference/ncei_datasets.md)
  : List available NCEI datasets
- [`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
  : Search for NCEI weather stations
- [`ncei_bbox()`](https://connorb.github.io/preMetabolizer/reference/ncei_bbox.md)
  : Compute a bounding box around a point
- [`cdo_datasets()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_datacategories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_datatypes()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_locationcategories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_locations()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_stations()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  [`cdo_data()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
  : Query the NCEI Climate Data Online (CDO) Web Services v2 API
- [`cdo_request_count()`](https://connorb.github.io/preMetabolizer/reference/cdo_request_count.md)
  [`cdo_reset_request_count()`](https://connorb.github.io/preMetabolizer/reference/cdo_request_count.md)
  : Inspect and reset the NCEI CDO session request counter
- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  : Get NOAA station information
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  : Find NOAA stations near a location
- [`get_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/get_ghcnh.md)
  : Get GHCNh hourly observations
- [`get_noaa_ghg()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_ghg.md)
  : Get NOAA global monthly mean greenhouse gas concentrations
- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  : Download NASA POWER hourly data
- [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md)
  : Get elevation from the USGS Elevation Point Query Service
- [`iem_networks()`](https://connorb.github.io/preMetabolizer/reference/iem_networks.md)
  : Get Iowa Environmental Mesonet network identifiers
- [`iem_stations()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
  [`iem_station()`](https://connorb.github.io/preMetabolizer/reference/iem_stations.md)
  : Get Iowa Environmental Mesonet station metadata
- [`iem_current()`](https://connorb.github.io/preMetabolizer/reference/iem_current.md)
  : Get current Iowa Environmental Mesonet observations
- [`iem_obhistory()`](https://connorb.github.io/preMetabolizer/reference/iem_obhistory.md)
  : Get one day of Iowa Environmental Mesonet observations
- [`iem_daily()`](https://connorb.github.io/preMetabolizer/reference/iem_daily.md)
  : Get Iowa Environmental Mesonet daily summaries
- [`ks_meso_vars()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_vars.md)
  : Fetch Kansas Mesonet variable metadata
- [`ks_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_timeseries.md)
  : Get Kansas Mesonet time-series data
- [`ks_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_stations.md)
  : Get Kansas Mesonet station information
- [`ks_meso_station_activity()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_station_activity.md)
  : Get Kansas Mesonet station activity
- [`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
  : Get most recent Kansas Mesonet data timestamp
- [`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
  : Get Kansas Mesonet FW13 data
- [`tex_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_stations.md)
  : Get TexMesonet station information
- [`tex_meso_current()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_current.md)
  : Get current TexMesonet data
- [`tex_meso_timeseries()`](https://connorb.github.io/preMetabolizer/reference/tex_meso_timeseries.md)
  : Get recent TexMesonet time-series data
- [`read_shp()`](https://connorb.github.io/preMetabolizer/reference/read_shp.md)
  : Read a shapefile or zipped shapefile

## Chemistry calculations

Calculate dissolved gas concentrations, solubility constants, and
related thermodynamic quantities.

- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  : Calculate dissolved oxygen saturation
- [`calc_CO2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2sat.md)
  : Calculate dissolved carbon dioxide saturation
- [`calc_CH4sat()`](https://connorb.github.io/preMetabolizer/reference/calc_CH4sat.md)
  : Calculate dissolved methane saturation
- [`calc_N2Osat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2Osat.md)
  : Calculate dissolved nitrous oxide saturation
- [`calc_N2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_N2sat.md)
  : Calculate dissolved nitrogen saturation
- [`calc_Arsat()`](https://connorb.github.io/preMetabolizer/reference/calc_Arsat.md)
  : Calculate dissolved argon saturation
- [`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
  : Calculate dissolved CO2 concentration in mol/kg
- [`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md)
  : Calculate dissolved CO2 concentration in mg/L
- [`calc_K0()`](https://connorb.github.io/preMetabolizer/reference/calc_K0.md)
  : Calculate the CO2 solubility coefficient
- [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md)
  : Saturation vapor pressure of water
- [`xCO2_to_pCO2()`](https://connorb.github.io/preMetabolizer/reference/xCO2_to_pCO2.md)
  : Convert CO2 mole fraction to partial pressure
- [`pCO2_to_xCO2()`](https://connorb.github.io/preMetabolizer/reference/pCO2_to_xCO2.md)
  : Convert CO2 partial pressure to mole fraction

## Physical property calculations

Calculate water density, depth from pressure, and solar radiation.

- [`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)
  : Calculate water density
- [`calc_water_height()`](https://connorb.github.io/preMetabolizer/reference/calc_water_height.md)
  : Calculate water height from pressure
- [`calc_par()`](https://connorb.github.io/preMetabolizer/reference/calc_par.md)
  : Calculate modeled photosynthetically active radiation

## Statistical utilities

Descriptive statistics and outlier detection for time series data.

- [`calc_bin_width()`](https://connorb.github.io/preMetabolizer/reference/calc_bin_width.md)
  : Calculate histogram bin width
- [`calc_cv()`](https://connorb.github.io/preMetabolizer/reference/calc_cv.md)
  : Calculate the coefficient of variation
- [`calc_mode()`](https://connorb.github.io/preMetabolizer/reference/calc_mode.md)
  : Calculate the mode of a vector
- [`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md)
  : Calculate flow exceedance probabilities
- [`rcpp_calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/rcpp_calc_exceedance_prob.md)
  : Calculate flow exceedance probabilities with C++
- [`flag_z()`](https://connorb.github.io/preMetabolizer/reference/flag_z.md)
  : Flag outliers with robust Z-scores

## Unit conversion

Convert between common units used in hydrology and atmospheric science.

- [`convert_flow()`](https://connorb.github.io/preMetabolizer/reference/convert_flow.md)
  : Convert stream discharge between units
- [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  : Convert barometric pressure between units
- [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md)
  : Correct barometric pressure for elevation change

## Time utilities

Work with meteorological seasons and solar time.

- [`get_season()`](https://connorb.github.io/preMetabolizer/reference/get_season.md)
  : Determine the astronomical season from a date
- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  : Fill missing rows in an even time series
- [`convert_to_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  [`convert_from_solar_time()`](https://connorb.github.io/preMetabolizer/reference/convert_to_solar_time.md)
  : Convert a datetime to local solar time

## Data

Built-in reference datasets.

- [`french_creek`](https://connorb.github.io/preMetabolizer/reference/french_creek.md)
  : French Creek stream metabolism data
- [`kings_discharge`](https://connorb.github.io/preMetabolizer/reference/kings_discharge.md)
  : Kings Creek daily water data
