#' Calculate dissolved CO2 concentration in mol/kg
#'
#' Converts a measured or modeled CO2 mole fraction to dissolved CO2
#' concentration using atmospheric pressure, water-column pressure, vapor
#' pressure, and the Weiss (1974) CO2 solubility coefficient.
#'
#' @param CO2_ppm Numeric vector. Mole fraction of CO2 in air in parts per
#'   million.
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param waterDepth_m Numeric vector. Water depth above the sensor in meters.
#' @param atmo_press Numeric vector. Atmospheric pressure at the water surface.
#' @param press_units Character string giving the units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units.
#' @param salinity Numeric vector. Salinity in practical salinity units.
#'   Defaults to freshwater (`0`).
#'
#' @return Numeric vector of dissolved CO2 concentration in mol/kg.
#'
#' @seealso [calc_CO2_mgL()], [xCO2_to_pCO2()], [calc_K0()]
#'
#' @examples
#' calc_CO2_molKg(
#'   CO2_ppm = 420,
#'   temp_water = 20,
#'   waterDepth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @export
calc_CO2_molKg <- function(
  CO2_ppm,
  temp_water,
  waterDepth_m,
  atmo_press,
  press_units,
  salinity = 0
) {
  # Pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(
    pressure = atmo_press,
    units = press_units
  )

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

#' Calculate dissolved CO2 concentration in mg/L
#'
#' Converts a measured or modeled CO2 mole fraction to dissolved CO2
#' concentration in mg/L. This is a volume-based companion to
#' [calc_CO2_molKg()] and uses water density to convert from mol/kg.
#'
#' @inheritParams calc_CO2_molKg
#'
#' @return Numeric vector of dissolved CO2 concentration in mg/L.
#'
#' @seealso [calc_CO2_molKg()], [xCO2_to_pCO2()], [calc_water_density()]
#'
#' @examples
#' calc_CO2_mgL(
#'   CO2_ppm = c(420, 800, 1200),
#'   temp_water = 20,
#'   waterDepth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @export
calc_CO2_mgL <- function(
  CO2_ppm,
  temp_water,
  waterDepth_m,
  atmo_press,
  press_units,
  salinity = 0
) {
  # Molar mass of CO2 in g/mol
  molar_mass_CO2 <- 44.0095 * 1e3 # g to mg
  # Pressure to atm
  atmo_press_atm <- convert_pressure_to_atm(
    pressure = atmo_press,
    units = press_units
  )

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
  density_kgL <- calc_water_density(water_temp = temp_water) / 1e3 #kg/m^3 to kg/L

  # Convert CO2 concentration from mol/kg to mg/L
  CO2_mgL <- CO2_molKg * molar_mass_CO2 * density_kgL
  return(CO2_mgL)
}
