mesonet_csv_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/csv"),
    body = charToRaw(body)
  )
}

mesonet_vars_response <- function() {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/html"),
    body = charToRaw(paste(
      "<script> jsonSchema = [",
      '{"TEMP2MAVG": { "var" : "AirTemperature.avg", "units": "°C", "desc": "Average air temperature at 2m" } },',
      '{"PRECIP": { "var": "Precipitation", "units": "mm", "desc": "Precipitation" } }',
      "];</script>"
    ))
  )
}

test_that("ks_meso_read_csv normalizes raw CSV data", {
  result <- ks_meso_read_csv(
    I(paste(
      "STATION,TIMESTAMP,TEMP2MAVG",
      "Manhattan,2024-01-01 00:00:00,10",
      sep = "\n"
    ))
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$station_name, "Manhattan")
  expect_equal(result$temp2_mavg, 10)
})

test_that("ks_meso_stations reads station metadata", {
  httr2::local_mocked_responses(function(req) {
    mesonet_csv_response(
      paste(
        "StationName,County,Latitude,Longitude,Elevation_m,Network,Abbreviation,OperatorName,FW13",
        "Manhattan,Riley,39.18,-96.58,310,KSRE,MHK,Kansas Mesonet,1",
        sep = "\n"
      )
    )
  })

  result <- ks_meso_stations()

  expect_s3_class(result, "tbl_df")
  expect_equal(result$station_name, "Manhattan")
  expect_equal(result$station_id, "MHK")
  expect_equal(result$network, "KSRE")
  expect_equal(result$network_name, "Kansas Mesonet")
})

test_that("ks_meso_most_recent requests the selected interval", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$query <- httr2::url_parse(req$url)$query
    mesonet_csv_response(
      paste(
        "STATION,TIMESTAMP",
        "--all--,2024-01-02 12:00:00",
        "Manhattan,2024-01-02 11:55:00",
        sep = "\n"
      )
    )
  })

  result <- ks_meso_most_recent("5min")

  expect_equal(env$query$int, "5min")
  expect_equal(result$station_name, c("--all--", "Manhattan"))
  expect_s3_class(result$timestamp, "POSIXct")
  expect_equal(attr(result$timestamp, "tzone"), "Etc/GMT+6")
})

test_that("ks_meso_fw13 requests station and compact dates", {
  env <- new.env()
  httr2::local_mocked_responses(function(req) {
    env$query <- httr2::url_parse(req$url)$query
    httr2::response(200, body = charToRaw("FW13 line 1\nFW13 line 2\n"))
  })

  result <- ks_meso_fw13("Ashland Bottoms", "2024-01-01", "20240102")

  expect_equal(env$query$stn, "Ashland Bottoms")
  expect_equal(env$query$t_start, "20240101")
  expect_equal(env$query$t_end, "20240102")
  expect_equal(result[1:2], c("FW13 line 1", "FW13 line 2"))
})

test_that("ks_meso_time_series can request stations or networks", {
  queries <- list()
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)

    if (grepl("stationnames", parsed$path)) {
      return(mesonet_csv_response(
        paste(
          "StationName,County,Latitude,Longitude,Elevation_m,Network,Abbreviation,OperatorName,FW13",
          "Ashland Bottoms,Riley,39.13,-96.64,320,KSRE,ASH,Kansas Mesonet,1",
          sep = "\n"
        )
      ))
    }

    if (grepl("variables", parsed$path)) {
      return(mesonet_vars_response())
    }

    queries[[length(queries) + 1]] <<- parsed$query
    mesonet_csv_response(
      paste(
        "STATION,TIMESTAMP,TEMP2MAVG,PRECIP",
        "Ashland Bottoms,2024-01-01 00:00:00,4,0",
        sep = "\n"
      )
    )
  })

  station_result <- ks_meso_time_series(
    stations = "Ashland Bottoms",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    interval = "day",
    vars = c("TEMP2MAVG", "PRECIP")
  )

  network_result <- ks_meso_time_series(
    network = "KSRE",
    start_date = "2024-01-01",
    end_date = "2024-01-01",
    interval = "day",
    vars = c("TEMP2MAVG", "PRECIP")
  )

  station_query <- queries[[1]]
  network_query <- queries[[2]]
  expect_equal(station_query$stn, "Ashland Bottoms")
  expect_equal(network_query$net, "KSRE")
  expect_equal(station_query$vars, "TEMP2MAVG,PRECIP")
  expect_equal(station_result$station_name, "Ashland Bottoms")
  expect_equal(station_result$station_id, "ASH")
  expect_equal(station_result$temp2_mavg, 4)
  expect_equal(network_result$network, "KSRE")
  expect_equal(network_result$precip, 0)
})

test_that("Mesonet timestamps are parsed as fixed Central Standard Time", {
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)

    if (grepl("stationnames", parsed$path)) {
      return(mesonet_csv_response(
        paste(
          "StationName,County,Latitude,Longitude,Elevation_m,Network,Abbreviation,OperatorName,FW13",
          "Manhattan,Riley,39.18,-96.58,310,KSRE,MHK,Kansas Mesonet,1",
          sep = "\n"
        )
      ))
    }

    if (grepl("variables", parsed$path)) {
      return(mesonet_vars_response())
    }

    mesonet_csv_response(
      paste(
        "STATION,TIMESTAMP,TEMP2MAVG",
        "Manhattan,2024-07-01 12:00:00,30",
        sep = "\n"
      )
    )
  })

  result <- ks_meso_time_series(
    stations = "Manhattan",
    start_date = "2024-07-01",
    end_date = "2024-07-01",
    interval = "day",
    vars = "TEMP2MAVG"
  )

  expect_equal(attr(result$timestamp, "tzone"), "Etc/GMT+6")
  expect_equal(
    as.numeric(result$timestamp),
    as.numeric(as.POSIXct("2024-07-01 18:00:00", tz = "UTC"))
  )
})

test_that("ks_meso_timeseries() is deprecated", {
  httr2::local_mocked_responses(function(req) {
    parsed <- httr2::url_parse(req$url)
    if (grepl("stationnames", parsed$path)) {
      return(mesonet_csv_response(
        paste(
          "StationName,County,Latitude,Longitude,Elevation_m,Network,Abbreviation,OperatorName,FW13",
          "Manhattan,Riley,39.18,-96.58,310,KSRE,MHK,Kansas Mesonet,1",
          sep = "\n"
        )
      ))
    }
    if (grepl("variables", parsed$path)) {
      return(mesonet_vars_response())
    }
    mesonet_csv_response(
      paste(
        "STATION,TIMESTAMP,TEMP2MAVG",
        "Manhattan,2024-01-01 00:00:00,10",
        sep = "\n"
      )
    )
  })

  expect_snapshot(
    invisible(ks_meso_timeseries(
      stations = "Manhattan",
      start_date = "2024-01-01",
      end_date = "2024-01-01",
      interval = "day",
      vars = "TEMP2MAVG"
    ))
  )
})
