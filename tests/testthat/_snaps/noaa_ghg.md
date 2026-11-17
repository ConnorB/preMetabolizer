# get_noaa_ghg validates inputs

    Code
      get_noaa_ghg("argon")
    Condition
      Error in `get_noaa_ghg()`:
      ! `gas` must be one of "co2", "ch4", "n2o", and "sf6".
      x Unknown gas: "argon".

---

    Code
      get_noaa_ghg(character())
    Condition
      Error in `get_noaa_ghg()`:
      ! `gas` must be a character vector.

---

    Code
      get_noaa_ghg("co2", units = "molar")
    Condition
      Error in `get_noaa_ghg()`:
      ! `units` must be one of "ppm", "ppb", or "ppt", not "molar".

---

    Code
      get_noaa_ghg("co2", quiet = "yes")
    Condition
      Error in `get_noaa_ghg()`:
      ! `quiet` must be `TRUE` or `FALSE`.

