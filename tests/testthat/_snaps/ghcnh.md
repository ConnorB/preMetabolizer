# get_ghcnh validates inputs

    Code
      get_ghcnh(stations = character(), start_date = "2023-01-01", end_date = "2023-01-31")
    Condition
      Error in `get_ghcnh()`:
      ! `stations` must be a non-empty character vector.

---

    Code
      get_ghcnh(stations = "USW00000001", start_date = "not-a-date", end_date = "2023-01-31")
    Condition
      Error in `get_ghcnh()`:
      ! `start_date` must be a valid date in `YYYY-MM-DD` format.

---

    Code
      get_ghcnh(stations = "USW00000001", start_date = "2023-12-31", end_date = "2023-01-01")
    Condition
      Error in `get_ghcnh()`:
      ! `start_date` must be on or before `end_date`.

