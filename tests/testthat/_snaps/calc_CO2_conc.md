# calc_K0 deprecates waterDepth_m argument

    Code
      invisible(calc_K0(20, waterDepth_m = 0.5))
    Condition
      Warning:
      The `waterDepth_m` argument of `calc_K0()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `water_depth_m` argument instead.

# calc_CO2_molKg deprecates waterDepth_m argument

    Code
      invisible(calc_CO2_molKg(CO2_ppm = 420, temp_water = 20, atmo_press = 101.325,
        press_units = "kPa", waterDepth_m = 0.5))
    Condition
      Warning:
      The `waterDepth_m` argument of `calc_CO2_molKg()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `water_depth_m` argument instead.

# calc_CO2_mgL deprecates waterDepth_m argument

    Code
      invisible(calc_CO2_mgL(CO2_ppm = 420, temp_water = 20, atmo_press = 101.325,
        press_units = "kPa", waterDepth_m = 0.5))
    Condition
      Warning:
      The `waterDepth_m` argument of `calc_CO2_mgL()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `water_depth_m` argument instead.

