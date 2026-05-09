# get_nasa_data requires coordinates for single-site data

    Code
      get_nasa_data(timeseries, lon = -96, elev_m = 300)
    Condition
      Error in `get_nasa_data()`:
      ! For single-site data, provide `lat` or include a single-valued lat column in `data`.

# get_nasa_data requires coordinates for multi-site data

    Code
      get_nasa_data(timeseries, site_col = "station_id")
    Condition
      Error in `get_nasa_data()`:
      ! `data` must contain lat, lon, and elev_m columns when `site_col` contains multiple sites.
      x Missing column: lat.

