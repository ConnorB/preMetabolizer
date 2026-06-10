# calc_vapor_press MIMSY warns for non-zero salinity

    Code
      calc_vapor_press(20, 10, "MIMSY")
    Condition
      Warning:
      `salinity` is ignored when `method = "MIMSY"`.
    Output
      [1] 0.02300875

# calc_vapor_press errors on invalid method

    Code
      calc_vapor_press(20, 0, "invalid")
    Condition
      Error in `calc_vapor_press()`:
      ! `method` must be one of "Dickson2007" or "MIMSY", not "invalid".

