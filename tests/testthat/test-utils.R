test_that("to_radians and to_degrees are inverse", {
  expect_equal(to_radians(180), pi)
  expect_equal(to_degrees(pi), 180)
  expect_equal(to_degrees(to_radians(45)), 45)
})

test_that("calc_declination_angle is ~0 near equinoxes", {
  # The formula's spring zero-crossing is jday 82; tolerance covers the
  # offset from the calendar equinox.
  expect_lt(abs(calc_declination_angle(82)), 0.1)
})

test_that("calc_declination_angle approaches axial tilt near summer solstice", {
  # Summer solstice is around day-of-year 172.
  expect_equal(calc_declination_angle(172), 23.439, tolerance = 0.1)
})

test_that("calc_declination_angle returns radians when requested", {
  deg <- calc_declination_angle(172, format = "degrees")
  rad <- calc_declination_angle(172, format = "radians")
  expect_equal(rad, to_radians(deg))
})

test_that("calc_hour_angle is 0 at solar noon", {
  expect_equal(calc_hour_angle(12), 0)
})

test_that("calc_hour_angle is +/- 90 degrees at 6 hours from noon", {
  expect_equal(calc_hour_angle(18), 90)
  expect_equal(calc_hour_angle(6), -90)
})

test_that("calc_zenith_angle is 0 when sun is overhead", {
  # Sun is directly overhead when latitude == declination and hour angle = 0.
  expect_equal(
    calc_zenith_angle(
      latitude = 23.439,
      declination.angle = 23.439,
      hour.angle = 0
    ),
    0,
    tolerance = 1e-6
  )
})

test_that("calc_zenith_angle accepts radians input", {
  z_deg <- calc_zenith_angle(40, 20, 30, format = "degrees")
  z_rad <- calc_zenith_angle(
    40,
    to_radians(20),
    to_radians(30),
    format = "radians"
  )
  expect_equal(z_rad, to_radians(z_deg))
})

test_that("calc_solar_insolation is ~max at solar noon on summer solstice", {
  noon <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  insol <- calc_solar_insolation(noon, latitude = 23.439)
  max_insol <- streamMetabolizer::convert_PAR_to_SW(2326)
  expect_equal(insol, max_insol, tolerance = 1e-3)
})

test_that("calc_solar_insolation is lower in winter than summer at noon", {
  summer_noon <- as.POSIXct("2024-06-21 12:00:00", tz = "UTC")
  winter_noon <- as.POSIXct("2024-12-21 12:00:00", tz = "UTC")
  expect_gt(
    calc_solar_insolation(summer_noon, latitude = 39.1),
    calc_solar_insolation(winter_noon, latitude = 39.1)
  )
})
