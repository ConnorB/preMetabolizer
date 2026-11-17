# correct_bp validates numeric and unit inputs

    Code
      correct_bp(station_bp = "bad", air_temp = 15, station_elev = 0, site_elev = 100)
    Condition
      Error in `correct_bp()`:
      ! `station_bp` must be numeric.

---

    Code
      correct_bp(station_bp = 101.3, air_temp = 15, station_elev = 0, site_elev = 100,
        from_units = "")
    Condition
      Error in `correct_bp()`:
      ! `from_units` must be a non-empty string.

