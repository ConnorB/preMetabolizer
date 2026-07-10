#' Calculate dissolved CO2 concentration in mol/kg
#'
#' Converts a measured or modeled CO2 mole fraction to dissolved CO2
#' concentration using atmospheric pressure, water-column pressure, vapor
#' pressure, and the Weiss (1974) CO2 solubility coefficient.
#'
#' @param co2_ppm Numeric vector. Mole fraction of CO2 in air in parts per
#'   million.
#' @param temp_water Numeric vector. Water temperature in degrees Celsius.
#' @param water_depth_m Numeric vector. Water depth above the sensor in meters.
#' @param atmo_press Numeric vector. Atmospheric pressure at the water surface.
#' @param press_units Character string giving the units of `atmo_press`. See
#'   [convert_pressure()] for accepted pressure units.
#' @param salinity Numeric vector. Salinity in practical salinity units.
#'   Defaults to freshwater (`0`).
#'
#' @return Numeric vector of dissolved CO2 concentration in mol/kg.
#'
#' @seealso [calc_co2_mg_l()], [xco2_to_pco2()], [calc_k0()]
#'
#' @examples
#' calc_co2_mol_kg(
#'   co2_ppm = 420,
#'   temp_water = 20,
#'   water_depth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @export
calc_co2_mol_kg <- function(
  co2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0
) {
  # Pressure to atm
  atmo_press_atm <- convert_pressure(
    pressure = atmo_press,
    from = press_units,
    to = "atm"
  )

  # Pressure correction to K0 uses the total (atmospheric + hydrostatic)
  # pressure at the sensor; calc_k0() converts water_depth_m internally.
  K0 <- calc_k0(temp_water, water_depth_m, atmo_press_atm, salinity)

  # Vapor pressure of water at the equilibrator
  vapor_press <- calc_vapor_press(temp_water, salinity)

  # Partial pressure of CO2 (atm). xCO2 [ppm] * (P - P_H2O) [atm] gives uatm,
  # then *1e-6 to atm.
  pCO2_atm <- (atmo_press_atm - vapor_press) * co2_ppm * 1e-6

  # Henry's law: [CO2*] = K0 * pCO2  (mol/kg)
  pCO2_atm * K0
}

#' Calculate dissolved CO2 concentration in mg/L
#'
#' Converts a measured or modeled CO2 mole fraction to dissolved CO2
#' concentration in mg/L. This is a volume-based companion to
#' [calc_co2_mol_kg()] and uses water density to convert from mol/kg.
#'
#' @inheritParams calc_co2_mol_kg
#'
#' @return Numeric vector of dissolved CO2 concentration in mg/L.
#'
#' @seealso [calc_co2_mol_kg()], [xco2_to_pco2()], [calc_water_density()]
#'
#' @examples
#' calc_co2_mg_l(
#'   co2_ppm = c(420, 800, 1200),
#'   temp_water = 20,
#'   water_depth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @export
calc_co2_mg_l <- function(
  co2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0
) {
  CO2_molKg <- calc_co2_mol_kg(
    co2_ppm = co2_ppm,
    temp_water = temp_water,
    water_depth_m = water_depth_m,
    atmo_press = atmo_press,
    press_units = press_units,
    salinity = salinity
  )

  # mg/mol for CO2, kg/L for water density
  molar_mass_CO2_mg <- 44.0095 * 1e3
  density_kgL <- calc_water_density(water_temp = temp_water) / 1e3

  CO2_molKg * molar_mass_CO2_mg * density_kgL
}

#' Calculate dissolved CO2 concentration in mol/kg (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [calc_co2_mol_kg()] instead.
#'
#' @param CO2_ppm `r lifecycle::badge("deprecated")` Use `co2_ppm` instead.
#' @param waterDepth_m `r lifecycle::badge("deprecated")` Use `water_depth_m`
#'   instead.
#'
#' @examples
#' # Old:
#' # calc_CO2_molKg(
#' #   CO2_ppm = 420,
#' #   temp_water = 20,
#' #   water_depth_m = 0.5,
#' #   atmo_press = 101.325,
#' #   press_units = "kPa"
#' # )
#' # New:
#' calc_co2_mol_kg(
#'   co2_ppm = 420,
#'   temp_water = 20,
#'   water_depth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @keywords internal
#' @export
calc_CO2_molKg <- function(
  CO2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0,
  waterDepth_m = lifecycle::deprecated()
) {
  lifecycle::deprecate_soft(
    "0.0.0.9000",
    "calc_CO2_molKg()",
    "calc_co2_mol_kg()"
  )
  if (lifecycle::is_present(waterDepth_m)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "calc_CO2_molKg(waterDepth_m)",
      "calc_co2_mol_kg(water_depth_m)"
    )
    water_depth_m <- waterDepth_m
  }

  calc_co2_mol_kg(
    co2_ppm = CO2_ppm,
    temp_water = temp_water,
    water_depth_m = water_depth_m,
    atmo_press = atmo_press,
    press_units = press_units,
    salinity = salinity
  )
}

#' Calculate dissolved CO2 concentration in mg/L (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [calc_co2_mg_l()] instead.
#'
#' @param CO2_ppm `r lifecycle::badge("deprecated")` Use `co2_ppm` instead.
#' @param waterDepth_m `r lifecycle::badge("deprecated")` Use `water_depth_m`
#'   instead.
#'
#' @examples
#' # Old:
#' # calc_CO2_mgL(
#' #   CO2_ppm = c(420, 800, 1200),
#' #   temp_water = 20,
#' #   water_depth_m = 0.5,
#' #   atmo_press = 101.325,
#' #   press_units = "kPa"
#' # )
#' # New:
#' calc_co2_mg_l(
#'   co2_ppm = c(420, 800, 1200),
#'   temp_water = 20,
#'   water_depth_m = 0.5,
#'   atmo_press = 101.325,
#'   press_units = "kPa"
#' )
#'
#' @keywords internal
#' @export
calc_CO2_mgL <- function(
  CO2_ppm,
  temp_water,
  water_depth_m,
  atmo_press,
  press_units,
  salinity = 0,
  waterDepth_m = lifecycle::deprecated()
) {
  lifecycle::deprecate_soft("0.0.0.9000", "calc_CO2_mgL()", "calc_co2_mg_l()")
  if (lifecycle::is_present(waterDepth_m)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "calc_CO2_mgL(waterDepth_m)",
      "calc_co2_mg_l(water_depth_m)"
    )
    water_depth_m <- waterDepth_m
  }

  calc_co2_mg_l(
    co2_ppm = CO2_ppm,
    temp_water = temp_water,
    water_depth_m = water_depth_m,
    atmo_press = atmo_press,
    press_units = press_units,
    salinity = salinity
  )
}
