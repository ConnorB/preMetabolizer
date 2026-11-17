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

---

    Code
      get_usgs_elev(latitude = 39, longitude = -96, units = c("m", "ft"))
    Condition
      Error in `get_usgs_elev()`:
      ! `units` must be a single string.

---

    Code
      get_usgs_elev(latitude = 39, longitude = -96, details = NA)
    Condition
      Error in `get_usgs_elev()`:
      ! `details` must be `TRUE` or `FALSE`.

# get_usgs_elev validates service responses

    Code
      result <- get_usgs_elev(latitude = 39, longitude = -96)
    Condition
      Warning:
      Could not retrieve USGS elevations for 1 point(s).
      i Input positions: 1.
      x USGS response did not include an elevation.
    Code
      expect_equal(result, NA_real_)

# get_usgs_elev rejects malformed and non-finite elevations

    Code
      result <- get_usgs_elev(latitude = c(39, 40), longitude = c(-96, -97))
    Condition
      Warning:
      Could not retrieve USGS elevations for 2 point(s).
      i Input positions: 1, 2.
      x USGS response did not include a finite elevation.
    Code
      expect_equal(result, c(NA_real_, NA_real_))

# get_usgs_elev reports permanent HTTP responses

    Code
      result <- get_usgs_elev(latitude = 39, longitude = -96, details = TRUE)
    Condition
      Warning:
      Could not retrieve USGS elevations for 1 point(s).
      i Input positions: 1.
      x USGS returned HTTP status 400.
    Code
      expect_equal(result$status, "http_error")
      expect_equal(result$http_status, 400)
      expect_equal(requests, 1)

# get_usgs_elev does not cache failed lookups

    Code
      first <- get_usgs_elev(latitude = 44, longitude = -100)
    Condition
      Warning:
      Could not retrieve USGS elevations for 1 point(s).
      i Input positions: 1.
      x USGS response did not include an elevation.

