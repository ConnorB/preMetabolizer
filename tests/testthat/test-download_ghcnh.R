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

  result <- read_ghcnh(files = path, quiet = TRUE)

  expect_equal(result$files_read, basename(path))
  expect_equal(result$files_failed, character())
  expect_s3_class(result$data, "data.frame")
  expect_equal(
    result$data$DateTime,
    as.POSIXct("2024-01-01 01:00:00", tz = "UTC")
  )
  expect_equal(result$data$temperature, 12.3)
  expect_equal("empty" %in% names(result$data), FALSE)
})

test_that("download_ghcnh reports files that are already cached", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- file.path(dir, "GHCNh_USW00000001_2024.csv.gz")
  writeLines("cached", path)

  result <- download_ghcnh(
    station_id = "USW00000001",
    years = 2024,
    output_dir = dir,
    quiet = TRUE
  )

  expect_equal(result$successful_downloads, character())
  expect_equal(result$skipped_downloads, "USW00000001_2024")
  expect_equal(result$total_attempted, 1)
  expect_equal(result$total_successful, 0)
  expect_equal(result$total_skipped, 1)
})

test_that("download_ghcnh validates inputs", {
  expect_snapshot(error = TRUE, {
    download_ghcnh(station_id = c("a", "b"), years = 2024)
  })
  expect_snapshot(error = TRUE, {
    download_ghcnh(station_id = "USW00000001", years = 2024.5)
  })
})

test_that("read_ghcnh reads files without Station_name", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  readr::write_csv(
    tibble::tibble(
      Station_ID = "USW00000001",
      DATE = "2024-01-01T01:00:00Z",
      temperature = -9999
    ),
    path
  )

  result <- read_ghcnh(files = path, quiet = TRUE)

  expect_named(result$data, c("DateTime", "Station_ID"))
  expect_equal(result$removed_cols, "temperature")
  expect_equal(result$stations, "USW00000001")
})

test_that("read_ghcnh validates inputs", {
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  path <- tempfile(fileext = ".csv")
  file.create(path)
  on.exit(unlink(path), add = TRUE)

  expect_snapshot(error = TRUE, {
    read_ghcnh(files = path, directory = dir)
  })
  expect_snapshot(error = TRUE, {
    read_ghcnh(files = "missing-ghcnh.csv")
  })
})
