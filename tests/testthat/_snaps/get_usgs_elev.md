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
      Error in `validate_coord()`:
      ! `latitude` contains invalid values.
      i Values must be finite and between -90 and 90.
      x Invalid positions: 1

# get_usgs_elev validates service responses

    Code
      result <- get_usgs_elev(latitude = 39, longitude = -96)
    Condition
      Warning:
      USGS elevation response did not include an elevation for point 1.
    Code
      expect_equal(result, NA_real_)

# get_usgs_elev does not cache failed lookups

    Code
      first <- get_usgs_elev(latitude = 44, longitude = -100)
    Condition
      Warning:
      USGS elevation response did not include an elevation for point 1.

