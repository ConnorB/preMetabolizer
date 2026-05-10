test_that("HTTP helper throttles requests by host", {
  req <- httr2::request("https://example.com/path") |>
    http_req_throttle()

  expect_equal(req$policies$throttle_realm, "example.com")
})

test_that("HTTP helper performs request lists", {
  urls <- character()
  httr2::local_mocked_responses(function(req) {
    urls <<- c(urls, req$url)
    httr2::response(200, body = charToRaw(req$url))
  })

  reqs <- list(
    httr2::request("https://example.com/a"),
    httr2::request("https://example.com/b")
  )

  responses <- http_req_perform_parallel(reqs)

  expect_equal(
    vapply(responses, httr2::resp_body_string, character(1)),
    c("https://example.com/a", "https://example.com/b")
  )
  expect_equal(urls, c("https://example.com/a", "https://example.com/b"))
})

test_that("HTTP helper performs single requests", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(200, body = charToRaw(req$url))
  })

  response <- http_req_perform(httr2::request("https://example.com/one"))

  expect_equal(
    httr2::resp_body_string(response),
    "https://example.com/one"
  )
})
