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
      ! `latitude` must be numeric.

---

    Code
      closest_noaa_stations(latitude = 39.1, longitude = -96.6, dist_km = -1)
    Condition
      Error in `closest_noaa_stations()`:
      ! `dist_km` must be a single positive number.

