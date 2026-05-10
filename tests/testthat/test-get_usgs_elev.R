json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

test_that("get_usgs_elev queries USGS elevation for one point", {
  env <- new.env()

  httr2::local_mocked_responses(function(req) {
    env$last_query <- httr2::url_parse(req$url)$query
    json_response('{"value":"335.0","rasterId":73412,"resolution":1}')
  })

  result <- get_usgs_elev(
    latitude = 39.102075,
    longitude = -96.5946888888889,
    units = "Meters"
  )

  expect_type(result, "double")
  expect_equal(length(result), 1)
  expect_equal(result, 335.0)

  expect_equal(
    as.numeric(env$last_query$x),
    -96.5946888888889,
    tolerance = 1e-5
  )
  expect_equal(as.numeric(env$last_query$y), 39.102075, tolerance = 1e-5)
  expect_equal(env$last_query$wkid, "4326")
  expect_equal(env$last_query$units, "Meters")
  expect_equal(env$last_query$includeDate, "false")
})

test_that("get_usgs_elev is vectorized over coordinate pairs", {
  queries <- list()

  httr2::local_mocked_responses(function(req) {
    q <- httr2::url_parse(req$url)$query
    queries[[length(queries) + 1]] <<- q
    elev <- as.character(as.numeric(q$y) + as.numeric(q$x))
    json_response(paste0('{"value":"', elev, '"}'))
  })

  result <- get_usgs_elev(
    latitude = c(39, 40),
    longitude = c(-96, -97),
    units = "Feet"
  )

  expect_type(result, "double")
  expect_equal(length(result), 2)
  expect_equal(result, c(39 - 96, 40 - 97))
})

test_that("get_usgs_elev validates coordinate inputs", {
  expect_snapshot(error = TRUE, {
    get_usgs_elev(latitude = c(39, 40), longitude = -96)
  })

  expect_snapshot(error = TRUE, {
    get_usgs_elev(latitude = 91, longitude = -96)
  })
})

test_that("get_usgs_elev validates service responses", {
  httr2::local_mocked_responses(function(req) {
    json_response('{"value":null}')
  })

  expect_snapshot({
    result <- get_usgs_elev(latitude = 39, longitude = -96)
    expect_equal(result, NA_real_)
  })
})
