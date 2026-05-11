ncei_stations_json <- function(results) {
  body <- list(totalCount = length(results), results = results)
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

fake_station <- function(
  id = "USW00023183",
  name = "LAS VEGAS INTL AP",
  lat = 36.085,
  lon = -115.147,
  startdate = "1948-01-01T00:00:00",
  enddate = "2024-01-15T23:59:59"
) {
  list(
    id = paste0("daily-summaries-latest.tar.gz:", id, ".csv"),
    name = paste0(id, ".csv"),
    startDate = startdate,
    endDate = enddate,
    centroid = list(point = list(lon, lat)),
    stations = list(list(
      id = id,
      name = name,
      dataTypes = list(),
      platforms = list()
    ))
  )
}

test_that("ncei_stations parses Search API response correctly", {
  httr2::local_mocked_responses(function(req) {
    ncei_stations_json(list(fake_station()))
  })

  result <- ncei_stations("daily-summaries")

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
  expect_equal(result$station_name, "LAS VEGAS INTL AP")
  expect_equal(result$latitude, 36.085)
  expect_equal(result$longitude, -115.147)
  expect_s3_class(result$start_date, "Date")
  expect_equal(result$start_date, as.Date("1948-01-01"))
})

test_that("ncei_stations sends bounding box when supplied", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    ncei_stations_json(list())
  })

  ncei_stations("daily-summaries", bbox = c(40, -100, 38, -98))

  expect_match(env$url, "bbox=")
  expect_match(env$url, "40")
  expect_match(env$url, "-100")
})

test_that("ncei_stations returns empty tibble when no hits", {
  httr2::local_mocked_responses(function(req) {
    ncei_stations_json(list())
  })

  result <- ncei_stations("daily-summaries")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
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
})

test_that("ncei_stations validates inputs", {
  expect_snapshot(error = TRUE, {
    ncei_stations(123)
  })
  expect_snapshot(error = TRUE, {
    ncei_stations("daily-summaries", bbox = c(38, -100, 40, -98))
  })
  expect_snapshot(error = TRUE, {
    ncei_stations("daily-summaries", limit = 0)
  })
})

test_that("ncei_bbox computes correct bounding box", {
  bbox <- ncei_bbox(39.1, -96.6, 50)

  expect_length(bbox, 4)
  expect_named(bbox, c("north", "west", "south", "east"))
  expect_gt(bbox["north"], 39.1)
  expect_lt(bbox["south"], 39.1)
  expect_gt(bbox["east"], -96.6)
  expect_lt(bbox["west"], -96.6)
})

test_that("ncei_bbox validates inputs", {
  expect_snapshot(error = TRUE, {
    ncei_bbox(100, -96.6, 50)
  })
  expect_snapshot(error = TRUE, {
    ncei_bbox(39.1, -96.6, -10)
  })
})
