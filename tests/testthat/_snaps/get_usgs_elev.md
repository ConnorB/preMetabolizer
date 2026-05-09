# get_usgs_elev validates coordinate inputs

    Code
      get_usgs_elev(latitude = c(39, 40), longitude = -96)
    Condition
      Error in `get_usgs_elev()`:
      ! `latitude` and `longitude` must have the same length.

---

    Code
      get_usgs_elev(latitude = 91, longitude = -96)
    Condition
      Error in `get_usgs_elev()`:
      ! `latitude` values must be between -90 and 90.

# get_usgs_elev validates service responses

    Code
      get_usgs_elev(latitude = 39, longitude = -96)
    Condition
      Error in `get_usgs_elev()`:
      ! USGS elevation response did not include a valid elevation for point 1.

