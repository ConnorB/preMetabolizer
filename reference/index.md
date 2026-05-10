# Package index

## Package overview

- [`preMetabolizer`](https://connorb.github.io/preMetabolizer/reference/preMetabolizer-package.md)
  [`preMetabolizer-package`](https://connorb.github.io/preMetabolizer/reference/preMetabolizer-package.md)
  : preMetabolizer: Prepare data for stream metabolism modeling

## Data access

Download and read meteorological and hydrological data from external
sources.

- [`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md)
  : Download GHCNh CSV files
- [`read_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/read_ghcnh.md)
  : Read GHCNh CSV files
- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  : Download NASA POWER hourly data
- [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md)
  : Get elevation from the USGS Elevation Point Query Service
- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  : Get NOAA station information
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  : Find NOAA stations near a location
- [`ks_meso_vars()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_vars.md)
  : Fetch Kansas Mesonet variable metadata
- [`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md)
  : Fetch data from Kansas Mesonet
- [`read_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/read_ks_meso.md)
  : Read cached Kansas Mesonet data
- [`ks_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_stations.md)
  : Get Kansas Mesonet station information
- [`ks_meso_station_activity()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_station_activity.md)
  : Get Kansas Mesonet station activity
- [`ks_meso_most_recent()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_most_recent.md)
  : Get most recent Kansas Mesonet data timestamp
- [`ks_meso_fw13()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_fw13.md)
  : Get Kansas Mesonet FW13 data
- [`read_shp()`](https://connorb.github.io/preMetabolizer/reference/read_shp.md)
  : Read a shapefile or zipped shapefile

## Chemistry calculations

Calculate dissolved gas concentrations, solubility constants, and
related thermodynamic quantities.

- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  : Calculate dissolved oxygen saturation
- [`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
  : Calculate dissolved CO2 concentration in mol/kg
- [`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md)
  : Calculate dissolved CO2 concentration in mg/L
- [`calc_K0()`](https://connorb.github.io/preMetabolizer/reference/calc_K0.md)
  : Calculate the CO2 solubility coefficient
- [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md)
  : Calculate water vapor pressure
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
- [`calc_light()`](https://connorb.github.io/preMetabolizer/reference/calc_light.md)
  : Calculate modeled photosynthetically active radiation
- [`calc_solar_insolation()`](https://connorb.github.io/preMetabolizer/reference/calc_solar_insolation.md)
  : Model solar insolation on a horizontal surface (W/m2 == J/s/m2) as
  in
  http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html

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
- [`convert_pressure_to_atm()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure_to_atm.md)
  : Convert barometric pressure to atmospheres
- [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md)
  : Correct barometric pressure for elevation change

## Time utilities

Work with meteorological seasons and solar time.

- [`get_season()`](https://connorb.github.io/preMetabolizer/reference/get_season.md)
  : Determine the season from a date
- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  : Fill missing rows in an even time series
- [`convert_UTC_to_solartime()`](https://connorb.github.io/preMetabolizer/reference/convert_UTC_to_solartime.md)
  : Convert UTC time to solar time
- [`convert_solartime_to_UTC()`](https://connorb.github.io/preMetabolizer/reference/convert_solartime_to_UTC.md)
  : Convert solar time to UTC

## Data

Built-in reference datasets.

- [`french_creek`](https://connorb.github.io/preMetabolizer/reference/french_creek.md)
  : French Creek stream metabolism data
- [`kings_discharge`](https://connorb.github.io/preMetabolizer/reference/kings_discharge.md)
  : Kings Creek daily water data
