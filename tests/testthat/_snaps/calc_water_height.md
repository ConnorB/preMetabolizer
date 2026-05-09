# calc_water_height errors when atmo_kPa is NULL for unvented type

    Code
      calc_water_height(120.5, water_temp = 15, type = "unvented")
    Condition
      Error in `calc_water_height()`:
      ! `atmo_kPa` must be provided when `type` is "unvented".

