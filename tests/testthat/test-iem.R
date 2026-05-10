iem_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

test_that("iem_networks reads network metadata", {
  httr2::local_mocked_responses(function(req) {
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"id"},{"name":"name"}]},',
      '"data":[{"id":"IA_ASOS","name":"Iowa ASOS",',
      '"windrose_update":"2026-05-10T19:00Z"}]}'
    ))
  })

  result <- iem_networks()

  expect_s3_class(result, "tbl_df")
  expect_equal(result$id, "IA_ASOS")
  expect_equal(result$name, "Iowa ASOS")
  expect_equal(attr(result$windrose_update, "tzone"), "UTC")
  expect_named(attr(result, "schema"), "fields")
})

test_that("iem_stations reads network station metadata", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"id"}]},',
      '"data":[{"id":"DSM","name":"Des Moines",',
      '"network":"IA_ASOS","archive_begin":"1928-08-01T00:00Z",',
      '"modified":"2026-05-10T19:00:01Z",',
      '"longitude":-93.65,"latitude":41.53}]}'
    ))
  })

  result <- iem_stations("IA_ASOS")

  expect_match(env$path, "/api/1/network/IA_ASOS.json$")
  expect_equal(result$id, "DSM")
  expect_equal(result$network, "IA_ASOS")
  expect_equal(attr(result$archive_begin, "tzone"), "UTC")
  expect_equal(attr(result$modified, "tzone"), "UTC")
})

test_that("iem_station reads station metadata across networks", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"id"}]},',
      '"data":[{"id":"AMW","name":"Ames","network":"IA_ASOS"},',
      '{"id":"AMW","name":"Ames","network":"IA_DCP"}]}'
    ))
  })

  result <- iem_station("AMW")

  expect_match(env$path, "/api/1/station/AMW.json$")
  expect_equal(result$network, c("IA_ASOS", "IA_DCP"))
})

test_that("iem_current builds filtered current observation requests", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    env$query <- parsed$query
    env$url <- req$url
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"station"}]},',
      '"data":[{"station":"DSM","network":"IA_ASOS",',
      '"utc_valid":"2026-05-10T19:05Z","local_date":"2026-05-10",',
      '"tmpf":84.2},{"station":"AMW","network":"IA_ASOS",',
      '"utc_valid":"2026-05-10T19:05:30Z",',
      '"local_date":"2026-05-10","tmpf":82.1}]}'
    ))
  })

  result <- iem_current(
    network = "IA_ASOS",
    stations = c("DSM", "AMW"),
    minutes = 120
  )

  expect_match(env$path, "/api/1/currents.json$")
  expect_equal(env$query$network, "IA_ASOS")
  expect_match(env$url, "station=DSM")
  expect_match(env$url, "station=AMW")
  expect_equal(env$query$minutes, "120")
  expect_equal(result$station, c("DSM", "AMW"))
  expect_equal(result$tmpf, c(84.2, 82.1))
  expect_equal(result$local_date, as.Date(c("2026-05-10", "2026-05-10")))
  expect_equal(attr(result$utc_valid, "tzone"), "UTC")
})

test_that("iem_obhistory reads one day of station observations", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    env$query <- parsed$query
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"station"}]},',
      '"data":[{"station":"DSM","utc_valid":"2024-06-01T05:00Z",',
      '"local_valid":"2024-06-01T00:00","tmpf":71.6,',
      '"sknt":8}]}'
    ))
  })

  result <- iem_obhistory(
    station = "DSM",
    network = "IA_ASOS",
    date = "2024-06-01",
    full = TRUE
  )

  expect_match(env$path, "/api/1/obhistory.json$")
  expect_equal(env$query$network, "IA_ASOS")
  expect_equal(env$query$station, "DSM")
  expect_equal(env$query$date, "2024-06-01")
  expect_equal(env$query$full, "true")
  expect_equal(result$tmpf, 71.6)
  expect_equal(attr(result$utc_valid, "tzone"), "UTC")
})

test_that("iem_daily reads daily summaries", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    env$path <- parsed$path
    env$query <- parsed$query
    iem_json_response(paste0(
      '{"schema":{"fields":[{"name":"station"}]},',
      '"data":[{"station":"DSM","day":"2024-06-01",',
      '"high":87,"low":64,"precip":0.12}]}'
    ))
  })

  result <- iem_daily("IA_ASOS", station = "DSM", year = 2024, month = 6)

  expect_match(env$path, "/api/1/daily.json$")
  expect_equal(env$query$network, "IA_ASOS")
  expect_equal(env$query$station, "DSM")
  expect_equal(env$query$year, "2024")
  expect_equal(env$query$month, "6")
  expect_equal(result$precip, 0.12)
})

test_that("iem functions validate inputs", {
  expect_snapshot(error = TRUE, {
    iem_stations("")
  })
  expect_snapshot(error = TRUE, {
    iem_current()
  })
  expect_snapshot(error = TRUE, {
    iem_obhistory("DSM", network = "IA_ASOS", date = "June 1, 2024")
  })
  expect_snapshot(error = TRUE, {
    iem_daily("IA_ASOS", date = "2024-06-01", year = 2024)
  })
})
