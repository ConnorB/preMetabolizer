# even_timesteps fills non-numeric columns from nearest observation

    Code
      result <- even_timesteps(df)
    Condition
      Warning:
      Column sonde is not numeric and cannot be interpolated.
      i Values are filled from the nearest observation within `max_gap / 2` seconds.

# even_timesteps drops missing timestamps with a warning

    Code
      result <- even_timesteps(df)
    Condition
      Warning:
      DateTime_UTC contains 1 missing value; dropping affected row.

# even_timesteps drops missing site values with a warning

    Code
      result <- even_timesteps(df, site_col = "site")
    Condition
      Warning:
      site contains 1 missing value; dropping affected row.

# even_timesteps errors without a datetime column

    Code
      even_timesteps(df)
    Condition
      Error in `even_timesteps()`:
      ! `logger_data` must contain a date-time column (<POSIXct>).
      i Alternatively, name the column with `datetime_col`.

# even_timesteps errors with multiple datetime columns

    Code
      even_timesteps(df)
    Condition
      Error in `even_timesteps()`:
      ! `logger_data` contains multiple date-time columns: start and end.
      i Specify the column to use with `datetime_col`.

# even_timesteps errors on missing explicit datetime column

    Code
      even_timesteps(df, datetime_col = "missing")
    Condition
      Error in `even_timesteps()`:
      ! `logger_data` must contain a missing column.

# even_timesteps errors on non-data.frame input

    Code
      even_timesteps(1:10)
    Condition
      Error in `even_timesteps()`:
      ! `logger_data` must be a data frame.

# even_timesteps errors on invalid max_gap

    Code
      even_timesteps(df, max_gap = -1)
    Condition
      Error in `even_timesteps()`:
      ! `max_gap` must be a single positive number of seconds.

# even_timesteps warns with a single timestamp

    Code
      result <- even_timesteps(df)
    Condition
      Warning:
      Data has fewer than two distinct non-missing timestamps; dropping 1 row.
      i An evenly spaced grid cannot be inferred.

# even_timesteps drops underdetermined sites instead of raw rows

    Code
      result <- even_timesteps(df, site_col = "site")
    Condition
      Warning:
      Site "B" has fewer than two distinct non-missing timestamps; dropping 1 row.
      i An evenly spaced grid cannot be inferred.

# even_timesteps deprecates loggerData argument

    Code
      invisible(even_timesteps(loggerData = df))
    Condition
      Warning:
      The `loggerData` argument of `even_timesteps()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `logger_data` argument instead.
