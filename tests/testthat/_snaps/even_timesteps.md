# even_timesteps errors on missing datetime column

    Code
      even_timesteps(df)
    Condition
      Error in `even_timesteps()`:
      ! Column 'DateTime_UTC' not found in input data frame

# even_timesteps errors on non-data.frame input

    Code
      even_timesteps(1:10)
    Condition
      Error in `even_timesteps()`:
      ! Input must be a data frame

