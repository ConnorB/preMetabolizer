# get_nasa_data errors with multiple datetime columns

    Code
      get_nasa_data(timeseries, latitude = 39, longitude = -96, elev_m = 300)
    Condition
      Error in `get_nasa_data()`:
      ! `data` contains multiple date-time columns: dateTime and solar_time.
      i Specify the column to use with `datetime_col`.

# get_nasa_data requires coordinates for single-site data

    Code
      get_nasa_data(timeseries, longitude = -96, elev_m = 300)
    Condition
      Error in `get_nasa_data()`:
      ! For single-site data, provide `latitude` or include a single-valued latitude column in `data`.

# get_nasa_data requires coordinates for multi-site data

    Code
      get_nasa_data(timeseries, site_col = "station_id")
    Condition
      Error in `get_nasa_data()`:
      ! `data` must contain latitude, longitude, and elev_m columns when `site_col` contains multiple sites.
      x Missing column: latitude.

# get_nasa_data warns about deprecated coordinate arguments

    Code
      result <- suppressMessages(get_nasa_data(timeseries, lat = 39, lon = -96,
        elev_m = 300))
    Condition
      Warning:
      The `lat` argument of `get_nasa_data()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `latitude` argument instead.
      Warning:
      The `lon` argument of `get_nasa_data()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `longitude` argument instead.

# get_nasa_data warns about deprecated coordinate columns

    Code
      result <- suppressMessages(get_nasa_data(timeseries))
    Condition
      Warning in `get_nasa_data()`:
      The lat column in `data` is deprecated as of preMetabolizer 0.1.0.
      i Use latitude instead.
      Warning in `get_nasa_data()`:
      The lon column in `data` is deprecated as of preMetabolizer 0.1.0.
      i Use longitude instead.
