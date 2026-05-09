ks_meso_station_data_request <- function(
  name,
  type,
  interval,
  time_start,
  time_end,
  vars
) {
  request <- httr2::request("http://mesonet.k-state.edu/rest/stationdata/") |>
    httr2::req_url_query(
      int = interval,
      t_start = time_start,
      t_end = time_end,
      vars = paste(vars, collapse = ",")
    ) |>
    httr2::req_retry(max_tries = 3)

  if (identical(type, "network")) {
    request |>
      httr2::req_url_query(net = name)
  } else {
    request |>
      httr2::req_url_query(stn = name)
  }
}

validate_ks_meso_dates <- function(start_date, end_date) {
  dates <- list(
    start = parse_ks_meso_date(start_date),
    end = parse_ks_meso_date(end_date)
  )

  if (is.na(dates$start) || is.na(dates$end)) {
    stop("start_date and end_date must be valid dates")
  }
  if (dates$start > dates$end) {
    stop("start_date must be before or equal to end_date")
  }

  dates
}

parse_ks_meso_date <- function(x) {
  parsed <- as.Date(x, format = "%Y-%m-%d")
  if (is.na(parsed)) {
    parsed <- as.Date(x, format = "%Y%m%d")
  }
  parsed
}

ks_meso_tz <- function() {
  "Etc/GMT+6"
}

read_ks_meso_csv <- function(file, na = c("", "NA")) {
  data <- readr::read_csv(
    file,
    na = na,
    col_types = readr::cols(
      TIMESTAMP = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  if ("TIMESTAMP" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      TIMESTAMP = parse_ks_meso_timestamp(.data$TIMESTAMP)
    )
  }

  data
}

parse_ks_meso_timestamp <- function(x) {
  utc_timestamp <- !is.na(x) & grepl("Z$", x)
  parsed <- as.POSIXct(x, tz = ks_meso_tz())

  if (any(utc_timestamp, na.rm = TRUE)) {
    utc_parsed <- as.POSIXct(
      x[utc_timestamp],
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
    parsed[utc_timestamp] <- as.POSIXct(
      format(utc_parsed, tz = ks_meso_tz(), usetz = FALSE),
      tz = ks_meso_tz()
    )
  }

  parsed
}

format_ks_meso_for_cache <- function(data) {
  if ("TIMESTAMP" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      TIMESTAMP = format(.data$TIMESTAMP, tz = ks_meso_tz(), usetz = FALSE)
    )
  }

  data
}
