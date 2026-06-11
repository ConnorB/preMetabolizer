# convert_to_solar_time deprecates dateTime argument

    Code
      invisible(convert_to_solar_time(dateTime = utc, longitude = -96.6))
    Condition
      Warning:
      The `dateTime` argument of `convert_to_solar_time()` is deprecated as of preMetabolizer 0.0.0.9000.
      i Please use the `date_time` argument instead.

# solar time conversions error on a bad longitude length

    Code
      convert_to_solar_time(utc, longitude = c(-96.6, -100, -110))
    Condition
      Error in `convert_to_solar_time()`:
      ! `longitude` must be length 1 or 2, not length 3.

