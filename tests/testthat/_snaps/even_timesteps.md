# even_timesteps errors on missing datetime column

    Code
      even_timesteps(df)
    Condition
      Error in `even_timesteps()`:
      ! `loggerData` must contain a DateTime_UTC column.

# even_timesteps errors on non-data.frame input

    Code
      even_timesteps(1:10)
    Condition
      Error in `even_timesteps()`:
      ! `loggerData` must be a data frame.

