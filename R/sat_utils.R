# Internal helpers shared by the calc_*sat() dissolved-gas saturation functions.

# Dry-pressure ratio that scales a moist-air 1-atm equilibrium concentration to
# the supplied barometric pressure: (P - pH2O) / (1 - pH2O), all in atm, using
# the salinity-aware Dickson et al. (2007) saturated water-vapor pressure.
# Shared by all six calc_*sat() functions.
sat_press_corr <- function(atmo_press, units, temp_water, salinity) {
  pressure_atm <- convert_pressure(atmo_press, from = units, to = "atm")
  P_H2O_atm <- calc_vapor_press(
    temp_water,
    salinity = salinity,
    method = "Dickson2007"
  )
  (pressure_atm - P_H2O_atm) / (1 - P_H2O_atm)
}

# Weiss & Price (1980) eq 13 solubility function F for a trace gas in moist air
# at 1 atm total pressure. Returns F in mol/(L atm) (volumetric) or mol/(kg atm)
# (gravimetric) depending on the supplied constants. Shared by calc_co2_sat()
# and calc_n2o_sat().
weiss_price_F <- function(temp_water, salinity, A1, A2, A3, A4, B1, B2, B3) {
  abs_temp <- temp_water + 273.15
  temp_ratio <- abs_temp / 100
  ln_F <- A1 +
    A2 * (100 / abs_temp) +
    A3 * log(temp_ratio) +
    A4 * temp_ratio^2 +
    salinity * (B1 + B2 * temp_ratio + B3 * temp_ratio^2)
  exp(ln_F)
}

# Hamme & Emerson (2004) eq 1 equilibrium concentration of a gas in water in
# contact with water-vapor-saturated air at 1 atm total pressure, using the
# eq 2 scaled temperature. Returns umol/kg with their Table 4 constants.
# Shared by calc_ar_sat() and calc_n2_sat().
hamme_emerson_sat <- function(
  temp_water,
  salinity,
  A0,
  A1,
  A2,
  A3,
  B0,
  B1,
  B2
) {
  # Scaled temperature (Hamme and Emerson 2004, eq 2)
  TS <- log((298.15 - temp_water) / (273.15 + temp_water))
  ln_C <- A0 +
    A1 * TS +
    A2 * TS^2 +
    A3 * TS^3 +
    salinity * (B0 + B1 * TS + B2 * TS^2)
  exp(ln_C)
}
