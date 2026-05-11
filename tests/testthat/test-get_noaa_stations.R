fake_ncei_stations_response <- function(station_id = "USW00023183") {
  body <- list(
    totalCount = 1L,
    results = list(list(
      id = paste0("daily-summaries-latest.tar.gz:", station_id, ".csv"),
      name = paste0(station_id, ".csv"),
      startDate = "1988-01-01T00:00:00",
      endDate = "2024-12-31T23:59:59",
      centroid = list(point = list(-96.59, 39.19)),
      stations = list(list(
        id = station_id,
        name = "MANHATTAN KS US",
        dataTypes = list(),
        platforms = list()
      ))
    ))
  )
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

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
      "name",
      "latitude",
      "longitude",
      "elevation",
      "start_date",
      "end_date",
      "data_coverage"
    )
  )
  expect_equal(result$station_id, "USW00023183")
  expect_equal(result$name, "MANHATTAN KS US")
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
