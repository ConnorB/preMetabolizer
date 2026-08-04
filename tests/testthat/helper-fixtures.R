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

json_response <- function(body, status = 200) {
  httr2::response(
    status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

clear_elev_cache <- function() {
  rm(list = ls(usgs_elev_cache), envir = usgs_elev_cache)
}

fake_noaa_stations <- function() {
  tibble::tibble(
    station_id = c("near", "far"),
    station_name = c("Near Station", "Far Station"),
    latitude = c(39.1, 42.0),
    longitude = c(-96.6, -100.0),
    start_date = as.Date(c("1990-01-01", "1995-01-01")),
    end_date = as.Date(c("2024-01-01", "2024-01-01"))
  )
}

tex_meso_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

make_point_sf <- function() {
  sf::st_as_sf(
    data.frame(id = 1L, x = -96.6, y = 39.1),
    coords = c("x", "y"),
    crs = 4326
  )
}

scrub_tempdir <- function(x) {
  gsub(
    paste0(normalizePath(tempdir(), winslash = "/"), "/[^'\"[:space:]]+"),
    "<tempdir>",
    gsub("\\\\", "/", x)
  )
}

fake_ncei_stations_response <- function(station_id = "USW00023183") {
  body <- list(
    totalCount = 1L,
    results = list(list(
      id = paste0("daily-summaries-latest.tar.gz:", station_id, ".csv"),
      name = paste0(station_id, ".csv"),
      startDate = "1988-01-01T00:00:00",
      endDate = "2024-12-31T23:59:59",
      centroid = list(point = list(-96.59, 39.19)),
      stations = list(list(
        id = station_id,
        name = "MANHATTAN KS US",
        dataTypes = list(),
        platforms = list()
      ))
    ))
  )
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

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

ncei_stations_json <- function(results) {
  body <- list(totalCount = length(results), results = results)
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
  )
}

fake_station <- function(
  id = "USW00023183",
  name = "LAS VEGAS INTL AP",
  lat = 36.085,
  lon = -115.147,
  startdate = "1948-01-01T00:00:00",
  enddate = "2024-01-15T23:59:59"
) {
  list(
    id = paste0("daily-summaries-latest.tar.gz:", id, ".csv"),
    name = paste0(id, ".csv"),
    startDate = startdate,
    endDate = enddate,
    centroid = list(point = list(lon, lat)),
    stations = list(list(
      id = id,
      name = name,
      dataTypes = list(),
      platforms = list()
    ))
  )
}

metab_plot_data <- function() {
  tibble::tibble(
    solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:2 * 3600,
    DO.obs = c(8, 8.2, 8.4),
    DO.sat = c(9, 0, 9.2),
    depth = c(0.4, 0.4, 0.4),
    temp.water = c(18, 18.2, 18.4),
    light = c(0, 100, 500)
  )
}

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
      "]; </script>"
    ))
  )
}

iem_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

ncei_csv_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "text/csv"),
    body = charToRaw(body)
  )
}

ncei_json_response <- function(body) {
  httr2::response(
    200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(body)
  )
}

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

x100 <- local({
  set.seed(42)
  rnorm(100)
})

calc_garcia_benson_mgL <- function(temp, salinity) {
  mgL_mlL <- 1.42905
  TS <- log((298.15 - temp) / (273.15 + temp))

  lnC <- 2.00907 +
    3.22014 * TS +
    4.05010 * TS^2 +
    4.94457 * TS^3 -
    2.56847e-1 * TS^4 +
    3.88767 * TS^5 -
    salinity *
      (6.24523e-3 +
        7.37614e-3 * TS +
        1.03410e-2 * TS^2 +
        8.17083e-3 * TS^3) -
    4.88682e-7 * salinity^2

  exp(lnC) * mgL_mlL
}
