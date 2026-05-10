test_that("get_noaa_stations filters cached raw data by state", {
  cache <- tempfile()
  dir.create(cache)
  old_options <- options(preMetabolizer.noaa_cache = cache)
  on.exit(options(old_options), add = TRUE)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  raw_data <- data.frame(
    STATE_PROV = c("KS", "CA"),
    GHCND_ID = c("USC001", "USC002")
  )
  saveRDS(raw_data, file.path(cache, "mshr_enhanced.rds"))

  local_mocked_bindings(
    get_remote_mtime = function(url) NULL
  )

  result <- get_noaa_stations(state = "ks", clean = FALSE, debug = FALSE)

  expect_equal(result$STATE_PROV, "KS")
  expect_equal(result$GHCND_ID, "USC001")
})

test_that("get_noaa_stations validates state and logical inputs", {
  expect_snapshot(error = TRUE, {
    get_noaa_stations(state = "Kansas", debug = FALSE)
  })
  expect_snapshot(error = TRUE, {
    get_noaa_stations(clean = c(TRUE, FALSE), debug = FALSE)
  })
})
