fake_noaa_stations <- function() {
  tibble::tibble(
    station_id = c("near", "far"),
    station_name = c("Near Station", "Far Station"),
    latitude = c(39.1, 42.0),
    longitude = c(-96.6, -100.0),
    start_date = as.Date(c("1990-01-01", "1995-01-01")),
    end_date = as.Date(c("2024-01-01", "2024-01-01"))
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

  expect_s3_class(result, "tbl_df")
  expect_equal(result$station_id, "near")
  expect_equal(result$distance_km, 0)
  expect_equal(names(result)[1], "distance_km")
  expect_true("station_name" %in% names(result))
})

test_that("closest_noaa_stations returns NULL when no stations in radius", {
  local_mocked_bindings(
    get_noaa_stations = function(...) {
      tibble::tibble(
        station_id = "far",
        station_name = "Far",
        latitude = 50.0,
        longitude = -96.6,
        start_date = as.Date(NA),
        end_date = as.Date(NA)
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
