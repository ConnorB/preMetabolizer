# calc_water_density warns for temperatures outside valid range

    Code
      calc_water_density(c(-1, 100, 200))
    Condition
      Warning:
      ! Some temperatures are outside the valid range.
      i Valid range is 0-150°C for freshwater (Tanaka et al. 2001 to 40°C, Kell 1975 above).
    Output
      [1] 999.7658 958.3637 863.6158

# calc_water_density warns outside the Millero and Poisson range

    Code
      calc_water_density(45, salinity = 35)
    Condition
      Warning:
      ! Some temperatures are outside the valid range.
      i Valid range is 0-40°C for the Millero and Poisson (1981) seawater equation.
    Output
      [1] 1015.865

---

    Code
      calc_water_density(20, salinity = 50)
    Condition
      Warning:
      ! Some salinities are outside the valid range.
      i Valid range is 0.5-43 for the Millero and Poisson (1981) seawater equation.
    Output
      [1] 1036.256

# calc_water_density errors on non-numeric input

    Code
      calc_water_density("warm")
    Condition
      Error in `calc_water_density()`:
      ! `water_temp` must be numeric.

---

    Code
      calc_water_density(15, salinity = "salty")
    Condition
      Error in `calc_water_density()`:
      ! `salinity` must be numeric.

