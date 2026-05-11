# ncei_stations validates inputs

    Code
      ncei_stations(123)
    Condition
      Error in `ncei_stations()`:
      ! `dataset` must be a single non-empty string.

---

    Code
      ncei_stations("daily-summaries", bbox = c(38, -100, 40, -98))
    Condition
      Error in `ncei_stations()`:
      ! `bbox`: north (38) must be >= south (40).

---

    Code
      ncei_stations("daily-summaries", limit = 0)
    Condition
      Error in `ncei_stations()`:
      ! `limit` must be a whole number between 1 and 1000.

# ncei_bbox validates inputs

    Code
      ncei_bbox(100, -96.6, 50)
    Condition
      Error in `ncei_bbox()`:
      ! `latitude` must be a single number between -90 and 90.

---

    Code
      ncei_bbox(39.1, -96.6, -10)
    Condition
      Error in `ncei_bbox()`:
      ! `dist_km` must be a single positive number.

