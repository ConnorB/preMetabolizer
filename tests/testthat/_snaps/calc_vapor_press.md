# calc_vapor_press MIMSY warns for non-zero salinity

    Code
      calc_vapor_press(20, 10, "MIMSY")
    Condition
      Warning in `calc_vapor_press()`:
      Salinity should be 0 when using the 'MIMSY' method
    Output
      [1] 0.02300875

# calc_vapor_press errors on invalid method

    Code
      calc_vapor_press(20, 0, "invalid")
    Condition
      Error in `calc_vapor_press()`:
      ! Invalid method. Choose 'Dickenson' or 'freshwater'.

