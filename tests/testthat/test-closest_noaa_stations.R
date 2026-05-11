fake_noaa_stations <- function() {
  tibble::tibble(
    station_id = c("near", "far"),
    name = c("Near Station", "Far Station"),
    latitude = c(39.1, 42.0),
    longitude = c(-96.6, -100.0),
    elevation = c(300.0, NA_real_),
    start_date = as.Date(c("1990-01-01", "1995-01-01")),
    end_date = as.Date(c("2024-01-01", "2024-01-01")),
    data_coverage = c(0.99, 0.85)
  )
}

test_that("closest_noaa_stations finds stations using standard coordinates", {
  local_mocked_bindings(
    get_noaa_stations = function(...) fake_noaa_stations()
  )

  result <- closest_noaa_stations(
    latitude = 39.1,
    longitude = -96.6,
    dist_km = 25
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$station_id, "near")
  expect_equal(result$distance_km, 0)
  expect_true("distance_km" %in% names(result))
})

test_that("closest_noaa_stations returns NULL when no stations in radius", {
  local_mocked_bindings(
    get_noaa_stations = function(...) {
      tibble::tibble(
        station_id = "far",
        name = "Far",
        latitude = 50.0,
        longitude = -96.6,
        elevation = NA_real_,
        start_date = as.Date(NA),
        end_date = as.Date(NA),
        data_coverage = NA_real_
      )
    }
  )

  expect_message(
    result <- closest_noaa_stations(
      latitude = 39.1,
      longitude = -96.6,
      dist_km = 25
    )
  )
  expect_null(result)
})

test_that("closest_noaa_stations warns about deprecated coordinate arguments", {
  old_options <- options(lifecycle_verbosity = "warning")
  on.exit(options(old_options), add = TRUE)

  local_mocked_bindings(
    get_noaa_stations = function(...) fake_noaa_stations()
  )

  expect_snapshot({
    result <- suppressMessages(closest_noaa_stations(
      lat = 39.1,
      long = -96.6,
      dist_km = 25
    ))
  })
  expect_equal(result$station_id, "near")
})

test_that("closest_noaa_stations warns about deprecated lon alias", {
  old_options <- options(lifecycle_verbosity = "warning")
  on.exit(options(old_options), add = TRUE)

  local_mocked_bindings(
    get_noaa_stations = function(...) fake_noaa_stations()
  )

  expect_snapshot({
    result <- suppressMessages(closest_noaa_stations(
      latitude = 39.1,
      lon = -96.6,
      dist_km = 25
    ))
  })
  expect_equal(result$station_id, "near")
})

test_that("closest_noaa_stations validates metadata shape", {
  local_mocked_bindings(
    get_noaa_stations = function(...) tibble::tibble(station_id = "bad")
  )

  expect_snapshot(error = TRUE, {
    closest_noaa_stations(
      latitude = 39.1,
      longitude = -96.6,
      dist_km = 25
    )
  })
})

test_that("closest_noaa_stations validates numeric inputs", {
  expect_snapshot(error = TRUE, {
    closest_noaa_stations(latitude = "a", longitude = -96.6, dist_km = 25)
  })
  expect_snapshot(error = TRUE, {
    closest_noaa_stations(latitude = 39.1, longitude = -96.6, dist_km = -1)
  })
})
