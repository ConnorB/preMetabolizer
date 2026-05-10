# get_season errors on invalid input

    Code
      get_season("not-a-date")
    Condition
      Error in `get_season()`:
      ! `date` must contain valid dates.
      Caused by error in `charToDate()`:
      ! character string is not in a standard unambiguous format

