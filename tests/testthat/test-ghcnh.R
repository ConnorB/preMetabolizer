ghcnh_psv_response <- function(psv_text) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/plain"),
    body = charToRaw(psv_text)
  )
}

psv_header <- paste(
  c(
    "Station_name",
    "Station_ID",
    "Year",
    "Month",
    "Day",
    "Hour",
    "Minute",
    "temperature",
    "dew_point_temperature",
    "relative_humidity"
  ),
  collapse = "|"
)

test_that("get_ghcnh downloads and parses PSV files", {
  psv <- paste(
    psv_header,
    paste(
      c(
        "Konza",
        "USW00000001",
        "2023",
        "01",
        "01",
        "01",
        "00",
        "21.5",
        "17.8",
        "78"
      ),
      collapse = "|"
    ),
    sep = "\n"
  )
  httr2::local_mocked_responses(function(req) ghcnh_psv_response(psv))

  result <- get_ghcnh(
    stations = "USW00000001",
    start_date = "2023-01-01",
    end_date = "2023-01-31",
    quiet = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_equal(result$station_id, "USW00000001")
  expect_equal(result$temperature, 21.5)
  expect_equal(result$dew_point_temperature, 17.8)
  expect_equal(
    result$datetime,
    as.POSIXct("2023-01-01 01:00:00", tz = "UTC")
  )
})

test_that("get_ghcnh converts -9999 to NA and drops all-NA columns", {
  psv <- paste(
    psv_header,
    paste(
      c(
        "Konza",
        "USW00000001",
        "2023",
        "01",
        "01",
        "00",
        "00",
        "-9999",
        "-9999",
        "78"
      ),
      collapse = "|"
    ),
    sep = "\n"
  )
  httr2::local_mocked_responses(function(req) ghcnh_psv_response(psv))

  result <- get_ghcnh(
    stations = "USW00000001",
    start_date = "2023-01-01",
    end_date = "2023-01-31",
    quiet = TRUE
  )

  expect_false("temperature" %in% names(result))
  expect_false("dew_point_temperature" %in% names(result))
})

test_that("ghcnh_build_requests emits informative message when not quiet", {
  expect_snapshot({
    reqs <- ghcnh_build_requests("USW00053974", 2024, quiet = FALSE)
  })
  expect_length(reqs, 1)
})

test_that("get_ghcnh validates inputs", {
  expect_snapshot(error = TRUE, {
    get_ghcnh(
      stations = character(),
      start_date = "2023-01-01",
      end_date = "2023-01-31"
    )
  })
  expect_snapshot(error = TRUE, {
    get_ghcnh(
      stations = "USW00000001",
      start_date = "not-a-date",
      end_date = "2023-01-31"
    )
  })
  expect_snapshot(error = TRUE, {
    get_ghcnh(
      stations = "USW00000001",
      start_date = "2023-12-31",
      end_date = "2023-01-01"
    )
  })
})
