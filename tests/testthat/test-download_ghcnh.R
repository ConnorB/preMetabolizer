test_that("read_ghcnh reads and standardizes CSV files", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  readr::write_csv(
    tibble::tibble(
      Station_name = "Konza",
      Station_ID = "USW00000001",
      DATE = "2024-01-01T01:00:00",
      temperature = 123,
      empty = NA_character_
    ),
    path
  )

  result <- read_ghcnh(files = path)

  expect_equal(result$files_read, basename(path))
  expect_equal(result$files_failed, character())
  expect_s3_class(result$data, "data.frame")
  expect_equal(
    result$data$DateTime,
    as.POSIXct("2024-01-01 01:00:00", tz = "UTC")
  )
  expect_equal(result$data$temperature, 12.3)
  expect_false("empty" %in% names(result$data))
})
