# calc_water_height errors when atmo_kpa is NULL for unvented type

    Code
      calc_water_height(120.5, water_temp = 15, type = "unvented")
    Condition
      Error in `calc_water_height()`:
      ! `atmo_kpa` must be provided when `type` is "unvented".

# calc_water_height(sensor_kPa) is deprecated

    Code
      invisible(calc_water_height(sensor_kPa = 19.2, water_temp = 15))
    Condition
      Warning:
      The `sensor_kPa` argument of `calc_water_height()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `sensor_kpa` argument instead.

# calc_water_height(atmo_kPa) is deprecated

    Code
      invisible(calc_water_height(sensor_kpa = 120.5, atmo_kPa = 101.3, water_temp = 15,
        type = "unvented"))
    Condition
      Warning:
      The `atmo_kPa` argument of `calc_water_height()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `atmo_kpa` argument instead.
