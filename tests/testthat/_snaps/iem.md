# iem_obhistory() is deprecated

    Code
      invisible(iem_obhistory(station = "DSM", network = "IA_ASOS"))
    Condition
      Warning:
      `iem_obhistory()` was deprecated in preMetabolizer 0.0.0.9000.
      i Please use `iem_ob_history()` instead.

# iem functions validate inputs

    Code
      iem_stations("")
    Condition
      Error in `iem_stations()`:
      ! `network` must be a non-empty string.

---

    Code
      iem_current()
    Condition
      Error in `iem_current()`:
      ! At least one of `network`, `networkclass`, `wfo`, `country`, `state`, or `stations` must be supplied.

---

    Code
      iem_ob_history("DSM", network = "IA_ASOS", date = "June 1, 2024")
    Condition
      Error in `iem_ob_history()`:
      ! `date` must be a valid date in `YYYY-MM-DD` format.

---

    Code
      iem_daily("IA_ASOS", date = "2024-06-01", year = 2024)
    Condition
      Error in `iem_daily()`:
      ! `date` cannot be supplied with `year` or `month`.

