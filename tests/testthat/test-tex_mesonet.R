tex_meso_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

test_that("tex_meso_stations reads and filters station metadata", {
  httr2::local_mocked_responses(function(req) {
    tex_meso_json_response(paste0(
      "[",
      '{"stationId":1,"stationName":"Old","displayId":"OLD",',
      '"state":"TX","county":"Blanco","latitude":30.1,',
      '"longitude":-98.4,"elevation":1334,"active":false,',
      '"stationType":1,"stationDisplay":false,"onlineDate":"4/28/2016"},',
      '{"stationId":2,"stationName":"Altwein Rd","displayId":"BCALT",',
      '"state":"TX","county":"Blanco","latitude":30.15,',
      '"longitude":-98.54,"elevation":1730,"active":true,',
      '"stationType":2,"stationDisplay":true,"onlineDate":"4/28/2016"}',
      "]"
    ))
  })

  result <- tex_meso_stations(active = TRUE, displayed = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$station_id, 2)
  expect_equal(result$station_name, "Altwein Rd")
  expect_equal(result$online_date, as.Date("2016-04-28"))
})

test_that("tex_meso_current reads data and units", {
  httr2::local_mocked_responses(function(req) {
    tex_meso_json_response(paste0(
      '{"units":{"airTemp":"Celsius","precipitation":"Millimeters"},',
      '"data":[{"stationId":2,"name":"Altwein Rd","displayId":"BCALT",',
      '"latitude":30.15,"longitude":-98.54,"airTemp":"29.69",',
      '"precip":"","recordedTime":"2026-05-10T18:45:00Z"}]}'
    ))
  })

  result <- tex_meso_current()

  expect_s3_class(result, "tbl_df")
  expect_equal(result$air_temp, 29.69)
  expect_equal(result$precip, NA_real_)
  expect_equal(result$station_name, "Altwein Rd")
  expect_equal(attr(result$recorded_time, "tzone"), "UTC")
  expect_equal(attr(result, "units")$air_temp, "Celsius")
})

test_that("tex_meso_timeseries reads all charting fields", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    tex_meso_json_response(paste0(
      '{"twdbStationId":2,"stationName":"Altwein Rd","values":[',
      '{"airTemp":"29.69","humidity":"49.93","precip":"0",',
      '"airPressure":"","dateTime":"2026-05-10T18:45:00Z"}]}'
    ))
  })

  result <- tex_meso_timeseries(2, prior_minutes = 60)

  expect_match(env$path, "/api/AllChartingFieldsById/2/60$")
  expect_equal(result$air_temp, 29.69)
  expect_equal(result$air_pressure, NA_real_)
  expect_equal(attr(result, "station_name"), "Altwein Rd")
  expect_equal(attr(result, "station_id"), 2)
  expect_equal(attr(result$date_time, "tzone"), "UTC")
})

test_that("tex_meso_timeseries reads single-variable fields", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    tex_meso_json_response(paste0(
      '{"twdbStationId":2,"stationName":"Altwein Rd",',
      '"fieldName":"Temperature","units":"Fahrenheit","values":[',
      '{"value":"85","dateTime":"2026-05-10T18:45:00Z"}]}'
    ))
  })

  result <- tex_meso_timeseries(
    2,
    prior_minutes = 60,
    variable = "temperature"
  )

  expect_match(env$path, "/api/TemperatureById/2/60$")
  expect_equal(result$value, 85)
  expect_equal(attr(result, "field_name"), "Temperature")
  expect_equal(attr(result, "units"), "Fahrenheit")
})

test_that("tex_meso functions validate inputs", {
  expect_snapshot(error = TRUE, {
    tex_meso_stations(active = NA)
  })
  expect_snapshot(error = TRUE, {
    tex_meso_timeseries(site_id = 2.5, prior_minutes = 60)
  })
  expect_snapshot(error = TRUE, {
    tex_meso_timeseries(site_id = 2, prior_minutes = 60, variable = "rain")
  })
})
