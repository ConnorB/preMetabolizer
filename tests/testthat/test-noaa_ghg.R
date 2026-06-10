noaa_ghg_response <- function(text) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/plain"),
    body = charToRaw(text)
  )
}

noaa_ghg_fixture <- function(gas) {
  rows <- switch(
    gas,
    co2 = c(
      "2020   1   2020.0417   413.40   0.10   412.50   -9.99",
      "2020   2   2020.1250   414.00   -9.99   413.00   0.08"
    ),
    ch4 = c(
      "2020   1   2020.0417   1875.50   1.20   1870.10   -9.99",
      "2020   2   2020.1250   1877.00   -9.99   1872.00   0.50"
    ),
    n2o = c(
      "2020   1   2020.0417   332.00   0.10   331.50   -9.99",
      "2020   2   2020.1250   332.50   -9.99   332.00   0.05"
    ),
    sf6 = c(
      "2020   1   2020.0417   10.50   0.02   10.40   -9.99",
      "2020   2   2020.1250   10.80   -9.99   10.60   0.01"
    )
  )
  paste(
    c(
      "# comment header",
      "# year month decimal average average_unc trend trend_unc",
      rows
    ),
    collapse = "\n"
  )
}

noaa_ghg_mock <- function(req) {
  gas <- sub(".*/([a-z0-9]+)_mm_gl\\.txt$", "\\1", req$url)
  noaa_ghg_response(noaa_ghg_fixture(gas))
}

test_that("get_noaa_ghg returns each gas in its standard unit by default", {
  httr2::local_mocked_responses(noaa_ghg_mock)

  result <- get_noaa_ghg(quiet = TRUE)

  expect_named(
    result,
    c("gas", "date", "unit", "average", "average_unc", "trend", "trend_unc")
  )
  expect_setequal(result$gas, c("CO2", "CH4", "N2O", "SF6"))
  expect_s3_class(result$date, "POSIXct")
  expect_equal(
    result$date[result$gas == "CO2"],
    lubridate::date_decimal(c(2020.0417, 2020.1250))
  )

  expect_equal(result$unit[result$gas == "CO2"], c("ppm", "ppm"))
  expect_equal(result$unit[result$gas == "CH4"], c("ppb", "ppb"))
  expect_equal(result$unit[result$gas == "N2O"], c("ppb", "ppb"))
  expect_equal(result$unit[result$gas == "SF6"], c("ppt", "ppt"))

  expect_equal(result$average[result$gas == "CO2"], c(413.40, 414.00))
  expect_equal(result$average[result$gas == "CH4"], c(1875.50, 1877.00))
  expect_equal(result$average[result$gas == "N2O"], c(332.00, 332.50))
  expect_equal(result$average[result$gas == "SF6"], c(10.50, 10.80))
})

test_that("get_noaa_ghg converts every gas to a requested common unit", {
  httr2::local_mocked_responses(noaa_ghg_mock)

  ppm <- get_noaa_ghg(units = "ppm", quiet = TRUE)
  expect_setequal(ppm$unit, "ppm")
  expect_equal(ppm$average[ppm$gas == "CO2"], c(413.40, 414.00))
  expect_equal(ppm$average[ppm$gas == "CH4"], c(1.87550, 1.87700))
  expect_equal(ppm$average[ppm$gas == "N2O"], c(0.33200, 0.33250))
  expect_equal(ppm$average[ppm$gas == "SF6"], c(10.50e-6, 10.80e-6))

  ppb <- get_noaa_ghg(units = "PPB", quiet = TRUE)
  expect_setequal(ppb$unit, "ppb")
  expect_equal(ppb$average[ppb$gas == "CO2"], c(413400, 414000))
  expect_equal(ppb$average[ppb$gas == "CH4"], c(1875.50, 1877.00))
  expect_equal(ppb$average[ppb$gas == "SF6"], c(0.01050, 0.01080))
})

test_that("get_noaa_ghg reads -9.9 uncertainty sentinels as NA", {
  httr2::local_mocked_responses(noaa_ghg_mock)

  result <- get_noaa_ghg("co2", quiet = TRUE)

  expect_equal(result$average_unc, c(0.10, NA))
  expect_equal(result$trend_unc, c(NA, 0.08))
})

test_that("get_noaa_ghg returns only the requested gases and is case insensitive", {
  httr2::local_mocked_responses(noaa_ghg_mock)

  result <- get_noaa_ghg(c("CH4", "co2", "ch4"), quiet = TRUE)

  expect_setequal(result$gas, c("CO2", "CH4"))
})

test_that("get_noaa_ghg validates inputs", {
  expect_snapshot(error = TRUE, get_noaa_ghg("argon"))
  expect_snapshot(error = TRUE, get_noaa_ghg(character()))
  expect_snapshot(error = TRUE, get_noaa_ghg("co2", units = "molar"))
  expect_snapshot(error = TRUE, get_noaa_ghg("co2", quiet = "yes"))
})
