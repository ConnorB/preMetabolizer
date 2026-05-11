# convert_UTC_to_solartime errors on non-POSIXct input

    Code
      convert_UTC_to_solartime("2024-06-21", -90)
    Condition
      Error in `convert_UTC_to_solartime()`:
      ! `date.time` must be a POSIXct vector.

# convert_UTC_to_solartime errors on non-UTC timezone

    Code
      convert_UTC_to_solartime(t_local, -90)
    Condition
      Error in `convert_UTC_to_solartime()`:
      ! `date.time` must have timezone "UTC".

# convert_solartime_to_UTC errors on non-POSIXct input

    Code
      convert_solartime_to_UTC("2024-06-21", -90)
    Condition
      Error in `convert_solartime_to_UTC()`:
      ! `any.solar.time` must be a POSIXct vector.

# convert_solartime_to_UTC errors on non-UTC timezone

    Code
      convert_solartime_to_UTC(t_local, -90)
    Condition
      Error in `convert_solartime_to_UTC()`:
      ! `any.solar.time` must have timezone "UTC".

