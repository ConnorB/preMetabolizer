#' Calculate CO2 Concentration from Sensor Data
#'
#' @param CO2_ppm Numeric. Mole fraction of CO2 in parts per million.
#' @param temp_water Numeric. Water temperature in degrees Celsius.
#' @param waterDepth_m Numeric. Water depth in meters.
#' @param atmo_press Numeric. Atmospheric pressure.
#' @param press_units Character. Units of atmospheric pressure.
#' @param salinity Numeric. Salinity in PSU.
#'
#' @return a numeric vector of CO2 concentration in mol/kg.
#' @export
calc_CO2_molKg <- function(CO2_ppm, temp_water, waterDepth_m, atmo_press, press_units, salinity = 0) {
  # Pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(pressure = atmo_press, units = press_units)

  # Water pressure in atm (convert depth in meters to atm; 10.1325 m = 1 atm)
  water_press_atm <- waterDepth_m / 10.1325

  # Total pressure (atmospheric + water)
  total_pressure <- atmo_press_atm + water_press_atm

  # Calculate K0 for water pressure
  K0 <- calc_K0(temp_water, waterDepth_m, atmo_press_atm, salinity)

  # Calculate vapor pressure of water
  vapor_press <- calc_vapor_press(temp_water, salinity)

  # Calculate partial pressure of CO2
  pCO2_uatm <- (atmo_press_atm - vapor_press) * CO2_ppm

  # uatm to atm
  pCO2_atm <- pCO2_uatm * 1e-6

  # Calculate CO2 concentration in water (mol/kg)
  CO2_molKg <- (pCO2_atm * (total_pressure) * K0)

  return(CO2_molKg)
}

#' Calculate CO2 Concentration from Sensor Data
#'
#' @param CO2_ppm Numeric. Mole fraction of CO2 in parts per million.
#' @param temp_water Numeric. Water temperature in degrees Celsius.
#' @param waterDepth_m Numeric. Water depth in meters.
#' @param atmo_press Numeric. Atmospheric pressure.
#' @param press_units Character. Units of atmospheric pressure.
#' @param salinity Numeric. Salinity in PSU.
#'
#' @return a numeric vector of CO2 concentration in mg/L.
#' @export
calc_CO2_mgL <- function(CO2_ppm, temp_water, waterDepth_m, atmo_press, press_units, salinity = 0){
  # Molar mass of CO2 in g/mol
  molar_mass_CO2 <- 44.0095 * 1e3 # g to mg
  # Pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(pressure = atmo_press, units = press_units)

  # Water pressure in atm (convert depth in meters to atm; 10.1325 m = 1 atm)
  water_press_atm <- waterDepth_m / 10.1325

  # Total pressure (atmospheric + water)
  total_pressure <- atmo_press_atm + water_press_atm

  # Calculate K0 for water pressure
  K0 <- calc_K0(temp_water, waterDepth_m, atmo_press_atm, salinity)

  # Calculate vapor pressure of water
  vapor_press <- calc_vapor_press(temp_water, salinity)

  # Calculate partial pressure of CO2
  pCO2_uatm <- (atmo_press_atm - vapor_press) * CO2_ppm

  # uatm to atm
  pCO2_atm <- pCO2_uatm * 1e-6

  # Calculate CO2 concentration in water (mol/kg)
  CO2_molKg <- (pCO2_atm * (total_pressure) * K0)

  # Water density
  density_kgL <- calc_water_density(tempC = temp_water)/1e3 #kg/m^3 to kg/L

  # Convert CO2 concentration from mol/kg to mg/L
  CO2_mgL <- CO2_molKg * molar_mass_CO2 * density_kgL
  return(CO2_mgL)
}
