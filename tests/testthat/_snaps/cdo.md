# cdo_token errors when API_NCEI_CDO is unset

    Code
      cdo_datasets()
    Condition
      Error in `cdo_request()`:
      ! NCEI CDO API token not found.
      i Set the `API_NCEI_CDO` environment variable.
      i Request a free token at <https://www.ncdc.noaa.gov/cdo-web/token>.

# cdo functions validate inputs

    Code
      cdo_data(datasetid = "GHCND", startdate = "bad", enddate = "2024-01-31")
    Condition
      Error in `cdo_data()`:
      ! `startdate` must be a valid date in `YYYY-MM-DD` format.

---

    Code
      cdo_stations(extent = c(0, 0, 0))
    Condition
      Error in `cdo_stations()`:
      ! `extent` must be a length-4 numeric vector: c(min_lat, min_lon, max_lat, max_lon).

---

    Code
      cdo_datasets(sortfield = "nonsense")
    Condition
      Error in `cdo_datasets()`:
      ! `sortfield` must be one of "id", "name", "mindate", "maxdate", or "datacoverage", not "nonsense".

---

    Code
      cdo_data(datasetid = "GHCND", startdate = "2024-01-01", enddate = "2024-01-31",
        max_results = -5)
    Condition
      Error in `cdo_data()`:
      ! `max_results` must be a single positive whole number or `Inf`.

# cdo_perform aborts when the daily session limit is exceeded

    Code
      cdo_datasets()
    Condition
      Error in `cdo_perform()`:
      ! Reached the NCEI CDO daily request limit of 10000 requests.
      i The limit resets daily; call `cdo_reset_request_count()` once you are within a new quota window.

