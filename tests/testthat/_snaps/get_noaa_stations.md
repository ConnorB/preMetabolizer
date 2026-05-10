# get_noaa_stations validates state and logical inputs

    Code
      get_noaa_stations(state = "Kansas", debug = FALSE)
    Condition
      Error in `get_noaa_stations()`:
      ! `state` must be a single two-letter state code.

---

    Code
      get_noaa_stations(clean = c(TRUE, FALSE), debug = FALSE)
    Condition
      Error in `get_noaa_stations()`:
      ! `clean` must be `TRUE` or `FALSE`.

