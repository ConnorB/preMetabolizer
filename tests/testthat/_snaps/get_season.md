# get_season errors on invalid input

    Code
      get_season("not-a-date")
    Condition
      Error in `get_season()`:
      ! `date` must contain valid dates.
      Caused by error in `charToDate()`:
      ! character string is not in a standard unambiguous format

# get_season errors on invalid labels

    Code
      get_season("2024-07-15", labels = "Summer")
    Condition
      Error in `get_season()`:
      ! `labels` must be a named character vector with names from "spring", "summer", "autumn", "winter".

---

    Code
      get_season("2024-07-15", labels = c(fall = "Fall"))
    Condition
      Error in `get_season()`:
      ! `labels` must be a named character vector with names from "spring", "summer", "autumn", "winter".
