# ghcnh_build_requests emits informative message when not quiet

    Code
      reqs <- ghcnh_build_requests("USW00053974", 2024, quiet = FALSE)
    Message
      Requesting GHCNh data for station "USW00053974", year 2024.

# get_ghcnh validates inputs

    Code
      get_ghcnh(stations = character(), start_date = "2023-01-01", end_date = "2023-01-31")
    Condition
      Error in `get_ghcnh()`:
      ! `stations` must be a character vector.

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

