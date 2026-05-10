fake_noaa_stations <- function() {
  data.frame(
    station_id = c("near", "far"),
    LAT_DEC = c(39.1, 42),
    LON_DEC = c(-96.6, -100),
    empty = c(NA, NA)
  )
}

test_that("closest_noaa_stations finds stations using standard coordinates", {
  calls <- list()

  local_mocked_bindings(
    get_noaa_stations = function(state = NULL, clean = TRUE) {
      calls[[length(calls) + 1]] <<- list(state = state, clean = clean)
      fake_noaa_stations()
    }
  )

  result <- closest_noaa_stations(
    latitude = 39.1,
    longitude = -96.6,
    dist_km = 25,
    state = "KS"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$station_id, "near")
  expect_equal(result$distance_km, 0)
  expect_named(result, c("distance_km", "station_id", "LAT_DEC", "LON_DEC"))
  expect_equal(calls[[1]], list(state = "KS", clean = TRUE))
})

test_that("closest_noaa_stations warns about deprecated coordinate arguments", {
  old_options <- options(lifecycle_verbosity = "warning")
  on.exit(options(old_options), add = TRUE)

  local_mocked_bindings(
    get_noaa_stations = function(state = NULL, clean = TRUE) {
      fake_noaa_stations()
    }
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
    get_noaa_stations = function(state = NULL, clean = TRUE) {
      fake_noaa_stations()
    }
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
    get_noaa_stations = function(state = NULL, clean = TRUE) {
      data.frame(station_id = "bad")
    }
  )

  expect_snapshot(error = TRUE, {
    closest_noaa_stations(
      latitude = 39.1,
      longitude = -96.6,
      dist_km = 25
    )
  })
})

test_that("closest_noaa_stations validates clean", {
  expect_snapshot(error = TRUE, {
    closest_noaa_stations(
      latitude = 39.1,
      longitude = -96.6,
      dist_km = 25,
      clean = NA
    )
  })
})
