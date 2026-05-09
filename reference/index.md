# Package index

## Data access

Download and read meteorological and hydrological data from external
sources.

- [`download_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/download_ghcnh.md)
  : Download GHCNh Parquet Files
- [`read_ghcnh()`](https://connorb.github.io/preMetabolizer/reference/read_ghcnh.md)
  : Read GHCNh Parquet Files
- [`get_nasa_data()`](https://connorb.github.io/preMetabolizer/reference/get_nasa_data.md)
  : Download NASA POWER hourly data
- [`get_usgs_elev()`](https://connorb.github.io/preMetabolizer/reference/get_usgs_elev.md)
  : Get elevation from the USGS Elevation Point Query Service
- [`get_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/get_noaa_stations.md)
  : Get NOAA Station Information
- [`closest_noaa_stations()`](https://connorb.github.io/preMetabolizer/reference/closest_noaa_stations.md)
  : Find NOAA Stations Within Specified Radius
- [`get_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/get_ks_meso.md)
  : Fetch Data from Kansas Mesonet
- [`read_ks_meso()`](https://connorb.github.io/preMetabolizer/reference/read_ks_meso.md)
  : Read Cached Kansas Mesonet Data
- [`ks_meso_stations()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_stations.md)
  : Get Kansas Mesonet Station Information
- [`ks_meso_station_activity()`](https://connorb.github.io/preMetabolizer/reference/ks_meso_station_activity.md)
  : Get Kansas Mesonet Station Activity
- [`read_shp()`](https://connorb.github.io/preMetabolizer/reference/read_shp.md)
  : Read Shapefile or Zipped Shapefile

## Chemistry calculations

Calculate dissolved gas concentrations, solubility constants, and
related thermodynamic quantities.

- [`calc_O2sat()`](https://connorb.github.io/preMetabolizer/reference/calc_O2sat.md)
  : Calculate Dissolved Oxygen Saturation
- [`calc_CO2_molKg()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_molKg.md)
  : Calculate CO2 Concentration from Sensor Data
- [`calc_CO2_mgL()`](https://connorb.github.io/preMetabolizer/reference/calc_CO2_mgL.md)
  : Calculate CO2 Concentration from Sensor Data
- [`calc_K0()`](https://connorb.github.io/preMetabolizer/reference/calc_K0.md)
  : Calculate Solubility Constant (K0) for CO2 in Water
- [`calc_vapor_press()`](https://connorb.github.io/preMetabolizer/reference/calc_vapor_press.md)
  : Calculate the vapor pressure of water at the supplied conditions
- [`xCO2_to_pCO2()`](https://connorb.github.io/preMetabolizer/reference/xCO2_to_pCO2.md)
  : Convert Mole Fraction of CO2 (xCO2) to Partial Pressure (pCO2)
- [`pCO2_to_xCO2()`](https://connorb.github.io/preMetabolizer/reference/pCO2_to_xCO2.md)
  : Convert p(CO2) to x(CO2)

## Physical property calculations

Calculate water density, depth from pressure, and solar radiation.

- [`calc_water_density()`](https://connorb.github.io/preMetabolizer/reference/calc_water_density.md)
  : Calculate Water Density
- [`calc_water_height()`](https://connorb.github.io/preMetabolizer/reference/calc_water_height.md)
  : Calculate Water Height from Sensor and Atmospheric Pressure
- [`calc_light()`](https://connorb.github.io/preMetabolizer/reference/calc_light.md)
  : Calculate modeled light from solar.time
- [`calc_solar_insolation()`](https://connorb.github.io/preMetabolizer/reference/calc_solar_insolation.md)
  : Model solar insolation on a horizontal surface (W/m2 == J/s/m2) as
  in
  http://education.gsfc.nasa.gov/experimental/July61999siteupdate/inv99Project.Site/Pages/solar.insolation.html

## Statistical utilities

Descriptive statistics and outlier detection for time series data.

- [`calc_bin_width()`](https://connorb.github.io/preMetabolizer/reference/calc_bin_width.md)
  : Calculate histogram bin width
- [`calc_cv()`](https://connorb.github.io/preMetabolizer/reference/calc_cv.md)
  : Calculate Coefficient of Variation
- [`calc_mode()`](https://connorb.github.io/preMetabolizer/reference/calc_mode.md)
  : Calculate the Mode(s) of a Vector
- [`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md)
  : Calculate Flow Exceedence Probabilities
- [`flag_z()`](https://connorb.github.io/preMetabolizer/reference/flag_z.md)
  : Flag Outliers Using Robust Z-Scores

## Unit conversion

Convert between common units used in hydrology and atmospheric science.

- [`convert_flow()`](https://connorb.github.io/preMetabolizer/reference/convert_flow.md)
  : Convert Stream Discharge Between Units
- [`convert_pressure()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure.md)
  : Convert Barometric Pressure Between Units
- [`convert_pressure_to_atm()`](https://connorb.github.io/preMetabolizer/reference/convert_pressure_to_atm.md)
  : Convert Barometric Pressure to Atmospheres
- [`correct_bp()`](https://connorb.github.io/preMetabolizer/reference/correct_bp.md)
  : Correct Barometric Pressure for Elevation Change

## Time utilities

Work with meteorological seasons and solar time.

- [`get_season()`](https://connorb.github.io/preMetabolizer/reference/get_season.md)
  : Determine the Season from a Date
- [`even_timesteps()`](https://connorb.github.io/preMetabolizer/reference/even_timesteps.md)
  : Get evenly spaced time steps for logger data
- [`convert_UTC_to_solartime()`](https://connorb.github.io/preMetabolizer/reference/convert_UTC_to_solartime.md)
  : Convert UTC Time to Mean or Apparent Solar Time
- [`convert_solartime_to_UTC()`](https://connorb.github.io/preMetabolizer/reference/convert_solartime_to_UTC.md)
  : Convert DateTime from Local Solar Time to UTC

## Data

Built-in reference datasets.

- [`ks_mesonet_vars`](https://connorb.github.io/preMetabolizer/reference/ks_mesonet_vars.md)
  : Kansas Mesonet Variables
- [`french_creek`](https://connorb.github.io/preMetabolizer/reference/french_creek.md)
  : French Creek Stream Metabolism Data
