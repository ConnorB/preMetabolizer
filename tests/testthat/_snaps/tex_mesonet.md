# tex_meso functions validate inputs

    Code
      tex_meso_stations(active = NA)
    Condition
      Error in `tex_meso_stations()`:
      ! `active` must be `TRUE` or `FALSE`.

---

    Code
      tex_meso_timeseries(site_id = 2.5, prior_minutes = 60)
    Condition
      Error in `tex_meso_timeseries()`:
      ! `site_id` must be a single positive whole number.

---

    Code
      tex_meso_timeseries(site_id = 2, prior_minutes = 60, variable = "rain")
    Condition
      Error in `tex_meso_timeseries()`:
      ! `variable` must be one of "all", "temperature", "humidity", "barometric_pressure", "precip", or "wind_speed", not "rain".

