ghcnh_base_url <-
  "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"

#' Get GHCNh hourly observations
#'
#' Downloads Global Historical Climatology Network hourly (GHCNh) data
#' for one or more stations and a date range, returning a parsed data
#' frame ready for use in stream metabolism modelling.
#'
#' @param stations Character vector of GHCNh station identifiers (e.g.,
#'   `"USW00023183"`). These match the `station_id` column returned by
#'   [get_noaa_stations()] and [closest_noaa_stations()].
#' @param start_date,end_date Start and end of the requested period as
#'   `Date` objects or `"YYYY-MM-DD"` strings.
#' @param quiet Logical. When `TRUE` progress messages are suppressed.
#'   Default `FALSE`.
#'
#' @return A [tibble][tibble::tibble-package] with one row per
#'   station-hour. Key columns include:
#'   \describe{
#'     \item{`station_id`}{Station identifier.}
#'     \item{`station_name`}{Station name (when available).}
#'     \item{`datetime`}{Observation time as a UTC `POSIXct`.}
#'     \item{`temperature`}{Air temperature in degrees Celsius.}
#'     \item{`dew_point_temperature`}{Dew-point temperature (°C).}
#'     \item{`relative_humidity`}{Relative humidity (%).}
#'     \item{`wind_direction`}{Wind direction (degrees clockwise from
#'       north).}
#'     \item{`wind_speed`}{Wind speed (m/s).}
#'     \item{`sea_level_pressure`}{Sea-level pressure (hPa).}
#'     \item{`precipitation`}{Precipitation (mm).}
#'   }
#'   Columns that are entirely `NA` across all stations and hours are
#'   dropped from the result.
#'
#' @details
#' GHCNh v1.1.0 data are served as pipe-separated (PSV) files from
#' the NOAA NCEI archive, one file per station per year. Data are
#' current through the present year. The function downloads all
#' station-year combinations covering the requested date range in
#' parallel.
#'
#' Sentinel values of `-9999` are converted to `NA`. Columns that
#' are all `NA` are removed.
#'
#' Multiple stations and years are downloaded in parallel using up
#' to four concurrent connections.
#'
#' @seealso [get_noaa_stations()], [closest_noaa_stations()],
#'   [ncei_data()]
#'
#' @examples
#' \dontrun{
#' # Fetch WY 2025 hourly data for a station near Konza Prairie
#' konza <- closest_noaa_stations(
#'   latitude = 39.1068806,
#'   longitude = -96.6117151,
#'   dist_km = 50
#' )
#' hourly <- get_ghcnh(
#'   stations = konza$station_id[1],
#'   start_date = "2024-10-01",
#'   end_date = "2025-09-30"
#' )
#' }
#'
#' @importFrom rlang .data
#' @export
get_ghcnh <- function(stations, start_date, end_date, quiet = FALSE) {
  if (
    missing(stations) ||
      !is.character(stations) ||
      length(stations) == 0 ||
      anyNA(stations) ||
      any(!nzchar(stations))
  ) {
    cli::cli_abort("{.arg stations} must be a non-empty character vector.")
  }
  start_date <- as.Date(ncei_check_date(start_date))
  end_date <- as.Date(ncei_check_date(end_date))
  if (start_date > end_date) {
    cli::cli_abort("{.arg start_date} must be on or before {.arg end_date}.")
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be `TRUE` or `FALSE`.")
  }

  years <- seq(
    as.integer(format(start_date, "%Y")),
    as.integer(format(end_date, "%Y"))
  )
  combos <- expand.grid(
    station_id = stations,
    year = years,
    stringsAsFactors = FALSE
  )

  reqs <- lapply(seq_len(nrow(combos)), function(i) {
    sid <- combos$station_id[i]
    year <- combos$year[i]
    url <- sprintf("%s/%d/GHCNh_%s_%d.psv", ghcnh_base_url, year, sid, year)
    if (!quiet) {
      cli::cli_inform(
        "Requesting GHCNh data for station {.val {sid}}, year {year}."
      )
    }
    httr2::request(url) |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 3)
  })

  resps <- http_req_perform_parallel(
    reqs,
    on_error = "return",
    progress = !quiet
  )

  dfs <- lapply(seq_along(resps), function(i) {
    resp <- resps[[i]]
    sid <- combos$station_id[i]
    year <- combos$year[i]

    if (inherits(resp, "error")) {
      cli::cli_warn(
        c(
          "Failed to download GHCNh data for station {.val {sid}}, year {year}.",
          "i" = conditionMessage(resp)
        )
      )
      return(NULL)
    }

    tryCatch(
      {
        body <- httr2::resp_body_string(resp)
        df <- readr::read_delim(
          I(body),
          delim = "|",
          show_col_types = FALSE,
          progress = FALSE
        )
        ghcnh_standardize(df)
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "Failed to parse GHCNh data for station {.val {sid}}, year {year}.",
            "i" = conditionMessage(e)
          )
        )
        NULL
      }
    )
  })

  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) {
    cli::cli_abort(
      "No GHCNh data could be retrieved for the requested stations and dates."
    )
  }

  combined <- dplyr::bind_rows(dfs)

  combined <- combined |>
    dplyr::filter(.data$datetime >= as.POSIXct(start_date, tz = "UTC")) |>
    dplyr::filter(.data$datetime < as.POSIXct(end_date + 1L, tz = "UTC"))

  if (nrow(combined) == 0) {
    cli::cli_abort(
      "No GHCNh data found for the requested stations and date range."
    )
  }

  all_na <- vapply(combined, function(x) all(is.na(x)), logical(1))
  combined[, !all_na]
}

ghcnh_standardize <- function(df) {
  quality_cols <- grep("_Quality_Code$", names(df), value = TRUE)
  for (col in quality_cols) {
    val <- df[[col]]
    if (is.data.frame(val)) {
      df[[col]] <- as.character(val[[1]])
    } else {
      df[[col]] <- as.character(val)
    }
  }

  code_cols <- grep(
    "(_Measurement_Code|_Report_Type|_Source_Code|_Source_Station_ID)$",
    names(df),
    value = TRUE
  )
  for (col in code_cols) {
    df[[col]] <- as.character(df[[col]])
  }

  numeric_vars <- c(
    "temperature",
    "dew_point_temperature",
    "station_level_pressure",
    "sea_level_pressure",
    "wind_direction",
    "wind_speed",
    "wind_gust",
    "precipitation",
    "relative_humidity",
    "wet_bulb_temperature",
    "snow_depth",
    "visibility",
    "altimeter",
    "pressure_3hr_change",
    paste0("precipitation_", c(3, 6, 9, 12, 15, 18, 21, 24), "_hour")
  )
  for (var in intersect(numeric_vars, names(df))) {
    df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
    df[[var]] <- ifelse(df[[var]] == -9999, NA_real_, df[[var]])
  }

  datetime <- ghcnh_parse_datetime(df)

  df <- df |>
    dplyr::mutate(datetime = datetime) |>
    dplyr::select(
      -dplyr::any_of(c("DATE", "Year", "Month", "Day", "Hour", "Minute"))
    )

  name_map <- c(
    Station_ID = "station_id",
    Station_name = "station_name"
  )
  for (old in intersect(names(name_map), names(df))) {
    names(df)[names(df) == old] <- name_map[[old]]
  }

  lead_cols <- intersect(
    c("station_id", "station_name", "datetime"),
    names(df)
  )
  df <- dplyr::relocate(df, dplyr::all_of(lead_cols))

  tibble::as_tibble(df)
}

ghcnh_parse_datetime <- function(df) {
  if ("DATE" %in% names(df)) {
    date <- df[["DATE"]]
    if (inherits(date, "POSIXt")) {
      return(as.POSIXct(date, tz = "UTC"))
    }
    dt <- suppressWarnings(lubridate::ymd_hms(date, tz = "UTC", quiet = TRUE))
    if (all(is.na(dt))) {
      dt <- suppressWarnings(lubridate::ymd_hm(date, tz = "UTC", quiet = TRUE))
    }
    return(dt)
  }

  if (all(c("Year", "Month", "Day", "Hour", "Minute") %in% names(df))) {
    return(as.POSIXct(
      sprintf(
        "%04d-%02d-%02d %02d:%02d:00",
        as.integer(df[["Year"]]),
        as.integer(df[["Month"]]),
        as.integer(df[["Day"]]),
        as.integer(df[["Hour"]]),
        as.integer(df[["Minute"]])
      ),
      tz = "UTC"
    ))
  }

  rep(as.POSIXct(NA, tz = "UTC"), nrow(df))
}
