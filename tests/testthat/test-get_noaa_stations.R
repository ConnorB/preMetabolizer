test_that("get_noaa_stations returns a tibble with expected columns", {
  httr2::local_mocked_responses(function(req) {
    fake_ncei_stations_response()
  })

  result <- get_noaa_stations(bbox = c(40, -100, 38, -98))

  expect_s3_class(result, "tbl_df")
  expect_named(
    result,
    c(
      "station_id",
      "station_name",
      "latitude",
      "longitude",
      "start_date",
      "end_date"
    )
  )
  expect_equal(result$station_id, "USW00023183")
  expect_equal(result$station_name, "MANHATTAN KS US")
})

test_that("get_noaa_stations passes parameters to ncei_stations", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    fake_ncei_stations_response()
  })

  get_noaa_stations(
    bbox = c(40, -98, 38, -96),
    start_date = "2000-01-01",
    data_types = "TMAX"
  )

  expect_match(env$url, "daily-summaries")
  expect_match(env$url, "2000-01-01")
  expect_match(env$url, "TMAX")
})
