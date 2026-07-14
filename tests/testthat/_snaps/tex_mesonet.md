# tex_meso functions validate inputs

    Code
      tex_meso_stations(active = NA)
    Condition
      Error in `tex_meso_stations()`:
      ! `active` must be `TRUE` or `FALSE`.

---

    Code
      tex_meso_time_series(site_id = 2.5, prior_minutes = 60)
    Condition
      Error in `tex_meso_time_series()`:
      ! `site_id` must be a single positive whole number.

---

    Code
      tex_meso_time_series(site_id = 2, prior_minutes = 60, variable = "rain")
    Condition
      Error in `tex_meso_time_series()`:
      ! `variable` must be one of "all", "temperature", "humidity", "barometric_pressure", "precip", or "wind_speed", not "rain".

# tex_meso_timeseries() is deprecated

    Code
      invisible(tex_meso_timeseries(2, prior_minutes = 60))
    Condition
      Warning:
      `tex_meso_timeseries()` was deprecated in preMetabolizer 0.0.0.9000.
      i Please use `tex_meso_time_series()` instead.
