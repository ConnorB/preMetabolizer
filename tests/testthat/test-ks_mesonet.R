test_that("read_ks_meso reads cached CSV data", {
  output_dir <- tempfile()
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  path <- file.path(
    output_dir,
    "Mesonet_Manhattan_hour_2024-01-01_to_2024-01-02.csv"
  )
  readr::write_csv(
    tibble::tibble(
      TIMESTAMP = as.POSIXct("2024-01-01 00:00:00", tz = "Etc/GMT-6"),
      TEMP2MAVG = 10
    ),
    path
  )

  result <- read_ks_meso(
    station = "Manhattan",
    start_date = "2024-01-01",
    end_date = "2024-01-02",
    interval = "hour",
    output_dir = output_dir
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$TEMP2MAVG, 10)
})
