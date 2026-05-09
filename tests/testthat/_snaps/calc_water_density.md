# calc_water_density warns for temperatures outside valid range

    Code
      calc_water_density(c(-1, 20, 151))
    Condition
      Warning:
      ! Some temperatures are outside the valid range.
      i Valid range is 0-150°C.
    Output
      [1] 999.7622 998.2041 915.8845

# calc_water_density errors on non-numeric input

    Code
      calc_water_density("warm")
    Condition
      Error in `calc_water_density()`:
      ! `water_temp` must be numeric.

