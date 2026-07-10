fake_nasa_power <- function(args) {
  datetimes <- seq(
    as.POSIXct(args$dates[[1]], tz = "UTC"),
    as.POSIXct(args$dates[[2]], tz = "UTC") + 23 * 60 * 60,
    by = "hour"
  )
  hours <- as.numeric(difftime(datetimes, datetimes[1], units = "hours"))

  data.frame(
    YYYYMMDD = format(datetimes, "%Y%m%d", tz = "UTC"),
    HR = as.integer(format(datetimes, "%H", tz = "UTC")),
    LON = args$lonlat[[1]],
    LAT = args$lonlat[[2]],
    YEAR = lubridate::year(datetimes),
    PSC = 101 + hours,
    ALLSKY_SFC_SW_DWN = hours,
    PRECTOTCORR = 0,
    T2M = 12
  )
}

test_that("get_nasa_data uses single-site coordinates and inferred dates", {
  timeseries <- data.frame(
    time = as.POSIXct(
      c("2024-01-02 00:00:00", "2024-01-04 12:00:00"),
      tz = "UTC"
    )
  )
  calls <- list()

  local_mocked_bindings(
    get_power = function(...) {
      args <- list(...)
      calls[[length(calls) + 1]] <<- args
      fake_nasa_power(args)
    }
  )

  result <- get_nasa_data(
    timeseries,
    datetime_col = "time",
    latitude = 39,
    longitude = -96,
    elev_m = 300
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(
    intersect(names(result), c("latitude", "longitude", "elev_m")),
    character()
  )
  expect_equal(calls[[1]]$lonlat, c(-96, 39))
  expect_equal(calls[[1]]$dates, as.Date(c("2024-01-02", "2024-01-04")))
  expect_equal(calls[[1]]$site_elev, 300)
  expect_s3_class(result$dateTime, "POSIXct")
  expect_equal(
    result$dateTime,
    lubridate::as_datetime(timeseries$time, tz = "UTC")
  )
  expect_equal(
    result$light.obs,
    result$ALLSKY_SFC_SW_DWN * 2.114
  )
})

test_that("get_nasa_data handles YEAR/MO/DY/HR hourly columns", {
  timeseries <- data.frame(
    time = as.POSIXct(
      c("2024-01-02 00:00:00", "2024-01-04 12:00:00"),
      tz = "UTC"
    )
  )

  local_mocked_bindings(
    get_power = function(...) {
      old <- fake_nasa_power(list(...))
      datetimes <- lubridate::ymd_h(paste(old$YYYYMMDD, old$HR), tz = "UTC")
      old$YYYYMMDD <- NULL
      old$MO <- lubridate::month(datetimes)
      old$DY <- lubridate::day(datetimes)
      old
    }
  )

  result <- get_nasa_data(
    timeseries,
    datetime_col = "time",
    latitude = 39,
    longitude = -96,
    elev_m = 300
  )

  expect_s3_class(result$dateTime, "POSIXct")
  expect_equal(
    result$dateTime,
    lubridate::as_datetime(timeseries$time, tz = "UTC")
  )
  expect_equal(
    intersect(names(result), c("YEAR", "MO", "DY", "HR")),
    character()
  )
})

test_that("get_nasa_data detects a datetime column with any name", {
  timeseries <- data.frame(
    timestamp = as.POSIXct(
      c("2024-01-02 00:00:00", "2024-01-04 12:00:00"),
      tz = "UTC"
    )
  )

  local_mocked_bindings(
    get_power = function(...) fake_nasa_power(list(...))
  )

  result <- get_nasa_data(
    timeseries,
    latitude = 39,
    longitude = -96,
    elev_m = 300
  )

  expect_equal(
    result$dateTime,
    lubridate::as_datetime(timeseries$timestamp, tz = "UTC")
  )
})

test_that("get_nasa_data errors with multiple datetime columns", {
  timeseries <- data.frame(
    dateTime = as.POSIXct("2024-01-02 00:00:00", tz = "UTC"),
    solar_time = as.POSIXct("2024-01-01 17:00:00", tz = "UTC")
  )
  expect_snapshot(
    error = TRUE,
    get_nasa_data(timeseries, latitude = 39, longitude = -96, elev_m = 300)
  )
})

test_that("get_nasa_data uses per-site coordinates and inferred dates", {
  timeseries <- data.frame(
    station_id = c("a", "a", "b", "b"),
    dateTime = as.POSIXct(
      c(
        "2024-01-02 00:00:00",
        "2024-01-03 12:00:00",
        "2024-02-01 00:00:00",
        "2024-02-05 12:00:00"
      ),
      tz = "UTC"
    ),
    latitude = c(39, 39, 40, 40),
    longitude = c(-96, -96, -97, -97),
    elev_m = c(300, 300, 350, 350)
  )
  calls <- list()

  local_mocked_bindings(
    get_power = function(...) {
      args <- list(...)
      calls[[length(calls) + 1]] <<- args
      fake_nasa_power(args)
    }
  )

  result <- get_nasa_data(timeseries, site_col = "station_id")

  expect_equal(result$station_id, c("a", "a", "b", "b"))
  expect_equal(result$dateTime, timeseries$dateTime)
  expect_equal(calls[[1]]$lonlat, c(-96, 39))
  expect_equal(calls[[1]]$dates, as.Date(c("2024-01-02", "2024-01-03")))
  expect_equal(calls[[1]]$site_elev, 300)
  expect_equal(calls[[2]]$lonlat, c(-97, 40))
  expect_equal(calls[[2]]$dates, as.Date(c("2024-02-01", "2024-02-05")))
  expect_equal(calls[[2]]$site_elev, 350)
})

test_that("get_nasa_data can read single-site coordinates from data", {
  timeseries <- data.frame(
    dateTime = as.Date(c("2024-01-01", "2024-01-02")),
    latitude = c(39, 39),
    longitude = c(-96, -96),
    elev_m = c(300, 300)
  )
  calls <- list()

  local_mocked_bindings(
    get_power = function(...) {
      args <- list(...)
      calls[[length(calls) + 1]] <<- args
      fake_nasa_power(args)
    }
  )

  get_nasa_data(timeseries)

  expect_equal(calls[[1]]$lonlat, c(-96, 39))
  expect_equal(calls[[1]]$site_elev, 300)
})

test_that("get_nasa_data can use scalar coordinates with a single site column", {
  timeseries <- data.frame(
    station_id = c("a", "a"),
    dateTime = as.Date(c("2024-01-01", "2024-01-02"))
  )
  calls <- list()

  local_mocked_bindings(
    get_power = function(...) {
      args <- list(...)
      calls[[length(calls) + 1]] <<- args
      fake_nasa_power(args)
    }
  )

  result <- get_nasa_data(
    timeseries,
    site_col = "station_id",
    latitude = 39,
    longitude = -96,
    elev_m = 300
  )

  expect_equal(result$station_id, c("a", "a"))
  expect_equal(calls[[1]]$lonlat, c(-96, 39))
  expect_equal(calls[[1]]$site_elev, 300)
})

test_that("get_nasa_data interpolates to input timestamps and converts shortwave", {
  timeseries <- data.frame(
    time = as.POSIXct(
      c(
        "2024-01-02 00:15:00",
        "2024-01-02 00:45:00",
        "2024-01-02 01:15:00"
      ),
      tz = "UTC"
    )
  )

  local_mocked_bindings(
    get_power = function(...) fake_nasa_power(list(...))
  )

  result <- get_nasa_data(
    timeseries,
    datetime_col = "time",
    latitude = 39,
    longitude = -96,
    elev_m = 300,
    params = c("PSC", "ALLSKY_SFC_SW_DWN")
  )

  expect_equal(result$dateTime, timeseries$time)
  expect_equal(result$PSC, c(101.25, 101.75, 102.25))
  expect_equal(result$ALLSKY_SFC_SW_DWN, c(0.25, 0.75, 1.25))
  expect_equal(
    result$light.obs,
    c(0.25, 0.75, 1.25) * 2.114
  )
})

test_that("get_nasa_data requires coordinates for single-site data", {
  timeseries <- data.frame(
    dateTime = as.Date("2024-01-01")
  )

  expect_snapshot(error = TRUE, {
    get_nasa_data(timeseries, longitude = -96, elev_m = 300)
  })
})

test_that("get_nasa_data requires coordinates for multi-site data", {
  timeseries <- data.frame(
    station_id = c("a", "b"),
    dateTime = as.Date(c("2024-01-01", "2024-01-01")),
    longitude = c(-96, -97),
    elev_m = c(300, 350)
  )

  expect_snapshot(error = TRUE, {
    get_nasa_data(timeseries, site_col = "station_id")
  })
})

test_that("get_nasa_data warns about deprecated coordinate arguments", {
  old_options <- options(lifecycle_verbosity = "warning")
  on.exit(options(old_options), add = TRUE)

  timeseries <- data.frame(
    dateTime = as.Date("2024-01-01")
  )

  local_mocked_bindings(
    get_power = function(...) fake_nasa_power(list(...))
  )

  expect_snapshot({
    result <- suppressMessages(get_nasa_data(
      timeseries,
      lat = 39,
      lon = -96,
      elev_m = 300
    ))
  })
  expect_equal(result$dateTime, lubridate::as_datetime(timeseries$dateTime))
})

test_that("get_nasa_data warns about deprecated coordinate columns", {
  timeseries <- data.frame(
    dateTime = as.Date("2024-01-01"),
    lat = 39,
    lon = -96,
    elev_m = 300
  )

  local_mocked_bindings(
    get_power = function(...) fake_nasa_power(list(...))
  )

  expect_snapshot({
    result <- suppressMessages(get_nasa_data(timeseries))
  })
  expect_equal(result$dateTime, lubridate::as_datetime(timeseries$dateTime))
})
