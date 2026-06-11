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

# even_timesteps deprecates loggerData argument

    Code
      invisible(even_timesteps(loggerData = df))
    Condition
      Warning:
      The `loggerData` argument of `even_timesteps()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `logger_data` argument instead.

