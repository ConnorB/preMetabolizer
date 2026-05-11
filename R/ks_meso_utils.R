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

ks_meso_build_timeseries_requests <- function(
  request_names,
  query_type,
  chunks,
  interval,
  vars
) {
  requests <- list()
  metadata <- list()

  for (request_name in request_names) {
    for (chunk in chunks) {
      time_start <- format(min(chunk), "%Y%m%d000000")
      time_end <- format(max(chunk), "%Y%m%d235959")
      request <- ks_meso_station_data_request(
        name = request_name,
        type = query_type,
        interval = interval,
        time_start = time_start,
        time_end = time_end,
        vars = vars
      )

      requests[[length(requests) + 1]] <- request
      metadata[[length(metadata) + 1]] <- list(
        request_name = request_name,
        query_type = query_type,
        start_time = time_start,
        end_time = time_end
      )
    }
  }

  list(requests = requests, metadata = metadata)
}

validate_ks_meso_dates <- function(start_date, end_date) {
  dates <- list(
    start = parse_ks_meso_date(start_date),
    end = parse_ks_meso_date(end_date)
  )

  if (is.na(dates$start) || is.na(dates$end)) {
    cli::cli_abort("{.arg start_date} and {.arg end_date} must be valid dates.")
  }
  if (dates$start > dates$end) {
    cli::cli_abort(
      "{.arg start_date} must be before or equal to {.arg end_date}."
    )
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

ks_meso_read_csv <- function(file, na = c("", "NA")) {
  header <- ks_meso_csv_header(file)
  col_names <- strsplit(header, ",", fixed = TRUE)[[1]]
  col_types <- if ("TIMESTAMP" %in% col_names) {
    readr::cols(
      TIMESTAMP = readr::col_character(),
      .default = readr::col_guess()
    )
  } else if ("timestamp" %in% col_names) {
    readr::cols(
      timestamp = readr::col_character(),
      .default = readr::col_guess()
    )
  } else {
    readr::cols(.default = readr::col_guess())
  }

  data <- readr::read_csv(
    file,
    na = na,
    col_types = col_types,
    show_col_types = FALSE
  )

  timestamp_col <- intersect(c("TIMESTAMP", "timestamp"), names(data))
  if (length(timestamp_col) > 0) {
    data[[timestamp_col[[1]]]] <- parse_ks_meso_timestamp(
      data[[timestamp_col[[1]]]]
    )
  }

  data |>
    mesonet_rename_columns(c(STATION = "station_name"))
}

ks_meso_csv_header <- function(file) {
  if (inherits(file, "AsIs")) {
    return(strsplit(as.character(file), "\n", fixed = TRUE)[[1]][[1]])
  }

  readr::read_lines(file, n_max = 1)
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
