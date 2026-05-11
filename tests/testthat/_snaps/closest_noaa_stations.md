# closest_noaa_stations warns about deprecated coordinate arguments

    Code
      result <- suppressMessages(closest_noaa_stations(lat = 39.1, long = -96.6,
        dist_km = 25))
    Condition
      Warning:
      The `lat` argument of `closest_noaa_stations()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `latitude` argument instead.
      Warning:
      The `long` argument of `closest_noaa_stations()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `longitude` argument instead.

# closest_noaa_stations warns about deprecated lon alias

    Code
      result <- suppressMessages(closest_noaa_stations(latitude = 39.1, lon = -96.6,
        dist_km = 25))
    Condition
      Warning:
      The `lon` argument of `closest_noaa_stations()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `longitude` argument instead.

# closest_noaa_stations validates metadata shape

    Code
      closest_noaa_stations(latitude = 39.1, longitude = -96.6, dist_km = 25)
    Condition
      Error in `closest_noaa_stations()`:
      ! `get_noaa_stations()` must return latitude and longitude columns.

# closest_noaa_stations validates numeric inputs

    Code
      closest_noaa_stations(latitude = "a", longitude = -96.6, dist_km = 25)
    Condition
      Error in `closest_noaa_stations()`:
      ! `latitude` must be a single finite number.

---

    Code
      closest_noaa_stations(latitude = 39.1, longitude = -96.6, dist_km = -1)
    Condition
      Error in `closest_noaa_stations()`:
      ! `dist_km` must be greater than 0.

