ncei_csv_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/csv"),
    body = charToRaw(body)
  )
}

ncei_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

test_that("ncei_data fetches and parses CSV from the Data Service API", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    ncei_csv_response(
      "STATION,DATE,NAME,TMAX,TMIN\nUSW00023183,2023-01-01,LAS VEGAS,155,56\n"
    )
  })

  result <- ncei_data(
    dataset = "daily-summaries",
    stations = "USW00023183",
    start_date = "2023-01-01",
    end_date = "2023-01-31",
    data_types = c("TMAX", "TMIN")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$STATION, "USW00023183")
  expect_equal(result$TMAX, 155)
  expect_match(env$url, "daily-summaries")
  expect_match(env$url, "USW00023183")
  expect_match(env$url, "2023-01-01")
  expect_match(env$url, "TMAX%2CTMIN|TMAX,TMIN")
})

test_that("ncei_data sends metric units by default", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    ncei_csv_response("STATION,DATE\nUSW00023183,2023-01-01\n")
  })

  ncei_data(
    dataset = "daily-summaries",
    stations = "USW00023183",
    start_date = "2023-01-01",
    end_date = "2023-01-01"
  )

  expect_match(env$url, "units=metric")
})

test_that("ncei_data validates inputs", {
  expect_snapshot(error = TRUE, {
    ncei_data(
      dataset = 1,
      stations = "x",
      start_date = "2023-01-01",
      end_date = "2023-12-31"
    )
  })
  expect_snapshot(error = TRUE, {
    ncei_data(
      dataset = "daily-summaries",
      stations = character(),
      start_date = "2023-01-01",
      end_date = "2023-12-31"
    )
  })
  expect_snapshot(error = TRUE, {
    ncei_data(
      dataset = "daily-summaries",
      stations = "x",
      start_date = "not-a-date",
      end_date = "2023-12-31"
    )
  })
})

test_that("ncei_datasets fetches dataset metadata", {
  httr2::local_mocked_responses(function(req) {
    ncei_json_response('{"id":"daily-summaries","name":"Daily Summaries"}')
  })

  result <- ncei_datasets("daily-summaries")

  expect_type(result, "list")
  expect_equal(result$id, "daily-summaries")
})

test_that("ncei_datasets validates inputs", {
  expect_snapshot(error = TRUE, {
    ncei_datasets(123)
  })
})
