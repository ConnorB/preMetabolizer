# even_timesteps errors on missing datetime column

    Code
      even_timesteps(df)
    Condition
      Error in `even_timesteps()`:
      ! `logger_data` must contain a DateTime_UTC column.

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

