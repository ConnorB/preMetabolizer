cdo_with_token <- function(value = "test-token") {
  old <- Sys.getenv("API_NCEI_CDO", unset = NA)
  Sys.setenv(API_NCEI_CDO = value)
  if (is.na(old)) {
    withr::defer(Sys.unsetenv("API_NCEI_CDO"), envir = parent.frame())
  } else {
    withr::defer(Sys.setenv(API_NCEI_CDO = old), envir = parent.frame())
  }
}

cdo_response <- function(body, status = 200) {
  httr2::response(
    status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

cdo_page <- function(results, count = NULL, offset = 1, limit = 1000) {
  if (is.null(count)) {
    count <- length(results)
  }
  list(
    metadata = list(
      resultset = list(offset = offset, count = count, limit = limit)
    ),
    results = results
  )
}

test_that("cdo_token errors when API_NCEI_CDO is unset", {
  old <- Sys.getenv("API_NCEI_CDO", unset = NA)
  Sys.unsetenv("API_NCEI_CDO")
  withr::defer({
    if (!is.na(old)) Sys.setenv(API_NCEI_CDO = old)
  })
  expect_snapshot(error = TRUE, cdo_datasets())
})

test_that("cdo_request injects token header", {
  cdo_with_token("abc123")
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$headers <- req$headers
    cdo_response(cdo_page(list()))
  })

  cdo_datasets()

  expect_equal(unname(env$headers$token), "abc123")
})

test_that("cdo_datasets parses list response into a tibble", {
  cdo_with_token()
  results <- list(
    list(
      uid = "gov.noaa.ncdc:C00861",
      mindate = "1763-01-01",
      maxdate = "2024-01-15",
      name = "Daily Summaries",
      datacoverage = 1,
      id = "GHCND"
    ),
    list(
      uid = "gov.noaa.ncdc:C00946",
      mindate = "1763-01-01",
      maxdate = "2024-01-15",
      name = "Global Summary of the Month",
      datacoverage = 1,
      id = "GSOM"
    )
  )
  httr2::local_mocked_responses(function(req) {
    cdo_response(cdo_page(results))
  })

  out <- cdo_datasets()

  expect_s3_class(out, "tbl_df")
  expect_equal(out$id, c("GHCND", "GSOM"))
  expect_s3_class(out$mindate, "Date")
  expect_equal(out$mindate[[1]], as.Date("1763-01-01"))
  expect_equal(out$datacoverage, c(1, 1))
})

test_that("cdo_datasets returns a list when id is supplied", {
  cdo_with_token()
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    cdo_response(list(
      id = "GHCND",
      name = "Daily Summaries",
      mindate = "1763-01-01",
      maxdate = "2024-01-15",
      datacoverage = 1
    ))
  })

  out <- cdo_datasets("GHCND")

  expect_type(out, "list")
  expect_equal(out$id, "GHCND")
  expect_match(env$url, "/datasets/GHCND")
})

test_that("cdo_data sends required filters and parses values", {
  cdo_with_token()
  env <- new.env()
  results <- list(
    list(
      date = "2024-01-01T00:00:00",
      datatype = "TMAX",
      station = "GHCND:USW00013722",
      attributes = ",,N,",
      value = 211
    ),
    list(
      date = "2024-01-01T00:00:00",
      datatype = "TMIN",
      station = "GHCND:USW00013722",
      attributes = ",,N,",
      value = 56
    )
  )
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    cdo_response(cdo_page(results))
  })

  out <- cdo_data(
    datasetid = "GHCND",
    stationid = "GHCND:USW00013722",
    startdate = "2024-01-01",
    enddate = "2024-01-31",
    datatypeid = c("TMAX", "TMIN")
  )

  expect_s3_class(out, "tbl_df")
  expect_s3_class(out$date, "POSIXct")
  expect_equal(out$value, c(211, 56))
  expect_match(env$url, "datasetid=GHCND")
  expect_match(env$url, "startdate=2024-01-01")
  expect_match(env$url, "enddate=2024-01-31")
  expect_match(env$url, "datatypeid=TMAX")
  expect_match(env$url, "datatypeid=TMIN")
})

test_that("cdo_paginate walks multiple pages and respects max_results", {
  cdo_with_token()
  pages <- list(
    cdo_page(
      lapply(1:1000, function(i) {
        list(
          id = sprintf("GHCND:ID%04d", i),
          name = sprintf("Station %04d", i),
          mindate = "2000-01-01",
          maxdate = "2024-01-01",
          latitude = 0,
          longitude = 0,
          elevation = 0,
          datacoverage = 1
        )
      }),
      count = 1500,
      offset = 1,
      limit = 1000
    ),
    cdo_page(
      lapply(1001:1500, function(i) {
        list(
          id = sprintf("GHCND:ID%04d", i),
          name = sprintf("Station %04d", i),
          mindate = "2000-01-01",
          maxdate = "2024-01-01",
          latitude = 0,
          longitude = 0,
          elevation = 0,
          datacoverage = 1
        )
      }),
      count = 1500,
      offset = 1001,
      limit = 1000
    )
  )
  page_idx <- 0L
  httr2::local_mocked_responses(function(req) {
    page_idx <<- page_idx + 1L
    cdo_response(pages[[page_idx]])
  })

  out <- cdo_stations(datasetid = "GHCND")

  expect_equal(nrow(out), 1500)
  expect_equal(page_idx, 2L)
})

test_that("cdo_paginate honours max_results cap", {
  cdo_with_token()
  results <- lapply(1:50, function(i) {
    list(
      id = sprintf("GHCND:ID%04d", i),
      name = sprintf("Station %04d", i),
      mindate = "2000-01-01",
      maxdate = "2024-01-01",
      latitude = 0,
      longitude = 0,
      elevation = 0,
      datacoverage = 1
    )
  })
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    cdo_response(cdo_page(results, count = 1500, limit = 50))
  })

  out <- cdo_stations(datasetid = "GHCND", max_results = 25)

  expect_equal(nrow(out), 25)
  expect_match(env$url, "limit=25")
})

test_that("cdo_stations validates and forwards extent bounding box", {
  cdo_with_token()
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    cdo_response(cdo_page(list()))
  })

  cdo_stations(extent = c(33.5, -85.0, 36.5, -75.0))

  expect_match(env$url, "extent=33\\.500000")
  expect_match(env$url, "%2C-75\\.000000")
})

test_that("cdo functions validate inputs", {
  cdo_with_token()
  expect_snapshot(error = TRUE, {
    cdo_data(datasetid = "GHCND", startdate = "bad", enddate = "2024-01-31")
  })
  expect_snapshot(error = TRUE, {
    cdo_stations(extent = c(0, 0, 0))
  })
  expect_snapshot(error = TRUE, {
    cdo_datasets(sortfield = "nonsense")
  })
  expect_snapshot(error = TRUE, {
    cdo_data(
      datasetid = "GHCND",
      startdate = "2024-01-01",
      enddate = "2024-01-31",
      max_results = -5
    )
  })
})

test_that("cdo_request_count increments on each successful request", {
  cdo_with_token()
  cdo_reset_request_count()
  withr::defer(cdo_reset_request_count())

  httr2::local_mocked_responses(function(req) {
    cdo_response(cdo_page(list()))
  })

  before <- cdo_request_count()
  cdo_datasets()
  cdo_datasets()
  expect_equal(cdo_request_count() - before, 2L)
})

test_that("cdo_perform aborts when the daily session limit is exceeded", {
  cdo_with_token()
  prior <- cdo_request_count()
  withr::defer({
    .cdo_state$count <- prior
    .cdo_state$warned <- FALSE
  })
  .cdo_state$count <- 10000L
  .cdo_state$warned <- TRUE

  httr2::local_mocked_responses(function(req) {
    cdo_response(cdo_page(list()))
  })

  expect_snapshot(error = TRUE, cdo_datasets())
})

test_that("cdo_request applies a throttle policy", {
  cdo_with_token()
  req <- cdo_request("/datasets")
  expect_equal(req$policies$throttle_realm, "ncei-cdo")
})

test_that("cdo_data accepts logical includemetadata and forwards as string", {
  cdo_with_token()
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$url <- req$url
    cdo_response(cdo_page(list()))
  })

  cdo_data(
    datasetid = "GHCND",
    startdate = "2024-01-01",
    enddate = "2024-01-31",
    includemetadata = TRUE
  )

  expect_match(env$url, "includemetadata=true")
})
