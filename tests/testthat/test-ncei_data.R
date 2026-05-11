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

  expect_s3_class(result, "tbl_df")
  expect_equal(result$station_id, "USW00023183")
  expect_equal(result$station_name, "LAS VEGAS")
  expect_equal(result$tmax, 155)
  expect_equal(result$tmin, 56)
  expect_s3_class(result$date, "Date")
  expect_equal(result$date, as.Date("2023-01-01"))
  expect_equal(names(result)[1:3], c("station_id", "station_name", "date"))
  expect_match(env$url, "daily-summaries")
  expect_match(env$url, "USW00023183")
  expect_match(env$url, "2023-01-01")
  expect_match(env$url, "TMAX%2CTMIN|TMAX,TMIN")
})

test_that("ncei_data parses global-hourly ISD packed fields", {
  body <- paste0(
    "STATION,DATE,WND,CIG,VIS,TMP,DEW,SLP\n",
    "72469023183,2023-06-01T00:00:00,\"010,1,N,0050,1\",",
    "\"00500,1,9,N\",\"016000,1,N,1\",",
    "\"+0241,1\",\"+0150,1\",\"10100,1\"\n",
    "72469023183,2023-06-01T01:00:00,\"999,9,9,9999,9\",",
    "\"99999,9,9,9\",\"999999,9,9,9\",",
    "\"+9999,9\",\"+9999,9\",\"99999,9\"\n"
  )
  httr2::local_mocked_responses(function(req) ncei_csv_response(body))

  result <- ncei_data(
    dataset = "global-hourly",
    stations = "72469023183",
    start_date = "2023-06-01",
    end_date = "2023-06-01"
  )

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result$datetime, "POSIXct")
  expect_equal(result$wind_direction, c(10, NA))
  expect_equal(result$wind_speed, c(5.0, NA))
  expect_equal(result$wind_speed_quality, c("1", "9"))
  expect_equal(result$ceiling_height, c(500, NA))
  expect_equal(result$visibility, c(16000, NA))
  expect_equal(result$temperature, c(24.1, NA))
  expect_equal(result$dew_point_temperature, c(15.0, NA))
  expect_equal(result$sea_level_pressure, c(1010.0, NA))
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
  expect_snapshot(error = TRUE, {
    ncei_data(
      dataset = "daily-summaries",
      stations = "x",
      start_date = "2023-01-01",
      end_date = "2023-12-31",
      units = "imperial"
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

test_that("ncei_expand_isd_fields handles all mandatory ISD fields", {
  df <- tibble::tibble(
    WND = c("010,1,N,0050,1", "999,9,9,9999,9"),
    CIG = c("00500,1,M,N", "99999,9,9,9"),
    VIS = c("016000,1,N,1", "999999,9,9,9"),
    TMP = c("+0241,1", "+9999,9"),
    DEW = c("-0050,1", "+9999,9"),
    SLP = c("10100,1", "99999,9"),
    AA1 = c("01,0025,1,1", "99,9999,9,9")
  )

  result <- ncei_expand_isd_fields(df)

  expect_equal(result$wind_direction, c(10, NA))
  expect_equal(result$wind_speed, c(5.0, NA))
  expect_equal(result$wind_type_code, c("N", "9"))
  expect_equal(result$ceiling_height, c(500, NA))
  expect_equal(result$visibility, c(16000, NA))
  expect_equal(result$temperature, c(24.1, NA))
  expect_equal(result$dew_point_temperature, c(-5.0, NA))
  expect_equal(result$sea_level_pressure, c(1010.0, NA))
  expect_equal(result$precipitation_period_hours, c(1, NA))
  expect_equal(result$precipitation, c(2.5, NA))
  expect_false("WND" %in% names(result))
  expect_false("AA1" %in% names(result))
})
