json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

clear_elev_cache <- function() {
  rm(list = ls(usgs_elev_cache), envir = usgs_elev_cache)
}

test_that("get_usgs_elev queries USGS elevation for one point", {
  clear_elev_cache()
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
  clear_elev_cache()
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
  clear_elev_cache()
  httr2::local_mocked_responses(function(req) {
    json_response('{"value":null}')
  })

  expect_snapshot({
    result <- get_usgs_elev(latitude = 39, longitude = -96)
    expect_equal(result, NA_real_)
  })
})

test_that("get_usgs_elev queries duplicated coordinate pairs once", {
  clear_elev_cache()
  n_requests <- 0

  httr2::local_mocked_responses(function(req) {
    n_requests <<- n_requests + 1
    q <- httr2::url_parse(req$url)$query
    elev <- as.character(as.numeric(q$y) + as.numeric(q$x))
    json_response(paste0('{"value":"', elev, '"}'))
  })

  result <- get_usgs_elev(
    latitude = c(41, 42, 41),
    longitude = c(-100, -105, -100)
  )

  expect_equal(n_requests, 2)
  expect_equal(result, c(-59, -63, -59))
})

test_that("get_usgs_elev caches results across calls", {
  clear_elev_cache()
  n_requests <- 0

  httr2::local_mocked_responses(function(req) {
    n_requests <<- n_requests + 1
    json_response('{"value":"335.0"}')
  })

  first <- get_usgs_elev(latitude = 43, longitude = -100)
  second <- get_usgs_elev(latitude = 43, longitude = -100)

  expect_equal(n_requests, 1)
  expect_equal(second, first)
})

test_that("get_usgs_elev does not cache failed lookups", {
  clear_elev_cache()

  httr2::local_mocked_responses(list(
    json_response('{"value":null}'),
    json_response('{"value":"12"}')
  ))

  expect_snapshot({
    first <- get_usgs_elev(latitude = 44, longitude = -100)
  })
  expect_equal(first, NA_real_)
  expect_equal(get_usgs_elev(latitude = 44, longitude = -100), 12)
})
