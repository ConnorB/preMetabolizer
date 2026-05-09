#' Fetch Data from Kansas Mesonet
#'
#' Retrieves weather data for specified stations from the Kansas Mesonet.
#'
#' @param stations Character vector of station names to retrieve data for. Use
#'   `"all"` to retrieve data for all stations. Must be `NULL` when `network`
#'   is supplied.
#' @param network Network name to retrieve data for, one of `"KSRE"`, `"BBW"`,
#'   or `"EBW"`. This is an alternative to `stations`.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` format.
#' @param interval Data interval. Must be one of `"hour"`, `"5min"`, or
#'   `"day"`.
#' @param vars Character vector of variables to retrieve. Defaults to common variables.
#' @param output_dir Directory to save the downloaded data. Defaults to the cache path.
#' @param debug Logical; if `TRUE`, debug messages are printed.
#'
#' @return A list with details about successful and failed downloads, output directory, and data chunks.
#'
#' @importFrom rlang .data
#'
#' @export
get_ks_meso <- function(
  stations = NULL,
  network = NULL,
  start_date,
  end_date,
  interval,
  vars = NULL,
  output_dir = NULL,
  debug = TRUE
) {
  interval <- rlang::arg_match(interval, c("hour", "5min", "day"))
  if (!is.logical(debug) || length(debug) != 1 || is.na(debug)) {
    stop("debug must be TRUE or FALSE")
  }
  if (!is.null(stations) && !is.null(network)) {
    stop("Only one of stations or network can be supplied")
  }
  if (is.null(stations) && is.null(network)) {
    stop("One of stations or network must be supplied")
  }
  if (
    !is.null(stations) && (!is.character(stations) || length(stations) == 0)
  ) {
    stop("stations must be a character vector")
  }
  if (!is.null(network)) {
    network <- rlang::arg_match(network, c("KSRE", "BBW", "EBW"))
  }

  dates <- validate_ks_meso_dates(start_date, end_date)

  # Use default variables if none provided
  vars <- if (is.null(vars)) {
    c(
      "PRESSUREAVG",
      "TEMP2MAVG",
      "TEMP2MMIN",
      "TEMP2MMAX",
      "RELHUM2MAVG",
      "PRECIP"
    )
  } else {
    vars
  }

  if (!is.null(stations)) {
    # Validate stations against available stations
    available_stations <- ks_meso_stations()$StationName
    stations_to_check <- setdiff(stations, "all")
    invalid_stations <- setdiff(stations_to_check, available_stations)
    if (length(invalid_stations) > 0) {
      stop(sprintf(
        "Invalid stations: %s",
        paste(invalid_stations, collapse = ", ")
      ))
    }
  }

  # Validate variables against available variables
  available_vars <- preMetabolizer::ks_mesonet_vars$Variable
  invalid_vars <- setdiff(vars, available_vars)
  if (length(invalid_vars) > 0) {
    stop(sprintf(
      "Invalid variables: %s",
      paste(invalid_vars, collapse = ", ")
    ))
  }

  # Handle output directory
  output_dir <- if (is.null(output_dir)) {
    path <- mesonet_cache()
    if (debug) {
      message("Using cache directory: ", path)
    }
    path
  } else {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_dir
  }
  # Calculate records per interval and max days per call
  records_per_interval <- list(
    hour = 24, # 24 records per day
    "5min" = 288, # 288 records per day (12 per hour * 24 hours)
    day = 1 # 1 record per day
  )[[interval]]

  # Calculate max days while leaving some buffer (90% of limit)
  max_days_per_call <- floor((3000 * 0.9) / records_per_interval)

  # Additional safety check for 5min interval
  if (interval == "5min") {
    max_days_per_call <- min(max_days_per_call, 10) # Force maximum of 10 days for 5min interval
  }

  # Initialize results collector
  results <- list(
    success = character(),
    failed = character(),
    chunks = list()
  )

  request_names <- if (is.null(network)) stations else network
  query_type <- if (is.null(network)) "station" else "network"

  # Process each station or network sequentially
  for (request_name in request_names) {
    if (debug) {
      message(sprintf("Processing %s: %s", query_type, request_name))
    }

    tryCatch(
      {
        output_file <- file.path(
          output_dir,
          sprintf(
            "Mesonet_%s_%s_%s_to_%s.csv",
            request_name,
            interval,
            start_date,
            end_date
          )
        )

        # Skip if file exists
        if (file.exists(output_file)) {
          if (debug) {
            message(sprintf("File exists: %s", output_file))
          }
          results$success <- c(results$success, request_name)
          next
        }

        # Generate date chunks with smaller intervals
        date_sequence <- seq(dates$start, dates$end, by = "days")
        chunks <- split(
          date_sequence,
          ceiling(seq_along(date_sequence) / max_days_per_call)
        )

        # Initialize list to store chunk data
        chunk_data <- vector("list", length(chunks))
        chunk_success <- logical(length(chunks))

        # Download data chunks
        for (i in seq_along(chunks)) {
          chunk <- chunks[[i]]
          time_start <- format(min(chunk), "%Y%m%d000000")
          time_end <- format(max(chunk), "%Y%m%d235959")

          request <- ks_meso_station_data_request(
            name = request_name,
            type = query_type,
            interval,
            time_start,
            time_end,
            vars
          )
          api_url <- request$url

          if (debug) {
            message(sprintf(
              "Downloading %s: %s to %s",
              request_name,
              time_start,
              time_end
            ))
          }

          # Try to download with automatic retry and error handling
          response <- tryCatch(
            {
              request |>
                httr2::req_perform()
            },
            error = function(e) {
              if (debug) {
                message(sprintf("Error in chunk %d: %s", i, e$message))
              }
              if (grepl("limit of 3000 records", e$message)) {
                max_days_per_call <<- floor(max_days_per_call * 0.75)
                if (debug) {
                  message(sprintf(
                    "Reducing max_days_per_call to %d",
                    max_days_per_call
                  ))
                }
              }
              return(NULL)
            }
          )

          if (!is.null(response)) {
            tryCatch(
              {
                temp_data <- read_ks_meso_csv(
                  I(httr2::resp_body_string(response)),
                  na = c("", "NA", "M")
                )

                chunk_data[[i]] <- temp_data
                chunk_success[i] <- TRUE

                results$chunks[[length(results$chunks) + 1]] <- list(
                  station = request_name,
                  start_time = time_start,
                  end_time = time_end,
                  url = api_url,
                  records = nrow(chunk_data[[i]])
                )
              },
              error = function(e) {
                if (debug) {
                  message(sprintf("Error parsing chunk %d: %s", i, e$message))
                }
                chunk_success[i] <- FALSE
              }
            )
          }

          Sys.sleep(0.5) # Rate limiting
        }

        # Remove failed chunks
        chunk_data <- chunk_data[chunk_success]

        # Combine chunks and write to CSV
        if (length(chunk_data) > 0 && any(sapply(chunk_data, nrow) > 0)) {
          combined_data <- dplyr::bind_rows(chunk_data)
          output_data <- format_ks_meso_for_cache(combined_data)
          readr::write_csv(output_data, output_file)

          results$success <- c(results$success, request_name)

          if (debug) message(sprintf("Data saved to: %s", output_file))
        } else {
          warning(sprintf(
            "No data retrieved for %s %s",
            query_type,
            request_name
          ))
          results$failed <- c(results$failed, request_name)
        }
      },
      error = function(e) {
        message(sprintf(
          "Error processing %s %s: %s",
          query_type,
          request_name,
          e$message
        ))
        results$failed <- c(results$failed, request_name)
      }
    )
  }

  # Return summary
  list(
    output_directory = normalizePath(output_dir),
    successful_downloads = results$success,
    failed_downloads = results$failed,
    total_attempted = length(request_names),
    total_successful = length(results$success),
    chunks = results$chunks
  )
}
#' Read Cached Kansas Mesonet Data
#'
#' Reads previously downloaded and cached Kansas Mesonet data.
#'
#' @param station Station name as a character string.
#' @param start_date Start date for the data in `YYYY-MM-DD` format.
#' @param end_date End date for the data in `YYYY-MM-DD` format.
#' @param interval Data interval. Must be one of `'hour'`, `'5min'`, or `'day'`.
#' @param output_dir Directory where the cached data is stored. Defaults to the cache path.
#'
#' @return A data frame containing the requested Mesonet data.
#'
#' @export
read_ks_meso <- function(
  station,
  start_date,
  end_date,
  interval,
  output_dir = NULL
) {
  if (missing(station) || !is.character(station)) {
    stop("station must be provided as a character string")
  }

  if (
    missing(start_date) ||
      missing(end_date) ||
      !inherits(lubridate::ymd(start_date), "Date") ||
      !inherits(lubridate::ymd(end_date), "Date")
  ) {
    stop(
      "start_date and end_date must be provided as valid date strings (YYYY-MM-DD)"
    )
  }

  if (missing(interval) || !is.character(interval)) {
    stop("interval must be provided as a character string")
  }

  if (is.null(output_dir)) {
    output_dir <- mesonet_cache()
  }

  # Create the expected file path
  output_file <- file.path(
    output_dir,
    sprintf(
      "Mesonet_%s_%s_%s_to_%s.csv",
      station,
      interval,
      start_date,
      end_date
    )
  )

  if (!file.exists(output_file)) {
    stop(sprintf(
      "Cached file for station %s, interval %s, and date range %s to %s does not exist.",
      station,
      interval,
      start_date,
      end_date
    ))
  }

  read_ks_meso_csv(output_file)
}

#' Get Kansas Mesonet Station Information
#'
#' Fetches metadata about Kansas Mesonet stations, including location and
#' network details.
#'
#' @return A data frame containing station metadata.
#'
#' @export
ks_meso_stations <- function() {
  url <- "http://mesonet.k-state.edu/rest/stationnames/"
  col_names <- c(
    "StationName",
    "County",
    "Latitude",
    "Longitude",
    "Elevation_m",
    "Network",
    "Abbreviation",
    "OperatorName",
    "FW13"
  )
  tryCatch(
    {
      response <- httr2::request(url) |> httr2::req_perform()
      content <- httr2::resp_body_string(response)
      station_names <- readr::read_csv(I(content), show_col_types = FALSE)
      if (ncol(station_names) == length(col_names)) {
        names(station_names) <- col_names
      }
      station_names
    },
    error = function(e) {
      stop(sprintf("Error fetching station names: %s", e$message))
    }
  )
}

#' Get Most Recent Kansas Mesonet Data Timestamp
#'
#' Retrieves the timestamp of the most recently ingested data for each station
#' for a given observation interval. The returned data includes the `"--all--"`
#' row reported by Kansas Mesonet.
#'
#' @param interval Data interval. Must be one of `"hour"`, `"5min"`, or
#'   `"day"`.
#'
#' @return A data frame with station names and most recent observation times.
#'
#' @export
ks_meso_most_recent <- function(interval) {
  interval <- rlang::arg_match(interval, c("hour", "5min", "day"))

  tryCatch(
    {
      response <- httr2::request(
        "http://mesonet.k-state.edu/rest/mostrecent"
      ) |>
        httr2::req_url_query(int = interval) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
      content <- httr2::resp_body_string(response)

      read_ks_meso_csv(I(content)) |>
        dplyr::transmute(
          station = .data$STATION,
          timestamp = .data$TIMESTAMP
        )
    },
    error = function(e) {
      stop(sprintf("Error fetching most recent Mesonet data: %s", e$message))
    }
  )
}

#' Get Kansas Mesonet FW13 Data
#'
#' Retrieves fire weather data in FW13 format for one station and date range.
#'
#' @param station Station name as a character string.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` or
#'   `YYYYMMDD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` or `YYYYMMDD`
#'   format.
#'
#' @return A character vector containing FW13 records.
#'
#' @export
ks_meso_fw13 <- function(station, start_date, end_date) {
  if (!is.character(station) || length(station) != 1 || is.na(station)) {
    stop("station must be a single character string")
  }

  dates <- validate_ks_meso_dates(start_date, end_date)

  tryCatch(
    {
      response <- httr2::request("http://mesonet.k-state.edu/rest/fw13") |>
        httr2::req_url_query(
          stn = station,
          t_start = format(dates$start, "%Y%m%d"),
          t_end = format(dates$end, "%Y%m%d")
        ) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()

      strsplit(httr2::resp_body_string(response), "\n", fixed = TRUE)[[1]]
    },
    error = function(e) {
      stop(sprintf("Error fetching FW13 Mesonet data: %s", e$message))
    }
  )
}

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

#' Get Kansas Mesonet Station Activity
#'
#' Retrieves activity data for Kansas Mesonet stations, including observation intervals and data spans.
#'
#' @return A data frame with station activity details, including start and end observation times.
#'
#'
#' @export
ks_meso_station_activity <- function() {
  # Define the URL
  url <- "http://mesonet.k-state.edu/rest/stationactive/"

  tryCatch(
    {
      # Fetch data with retry capability
      response <- httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
      content <- httr2::resp_body_string(response)

      interval_map <- c(
        "300" = "5min",
        "3600" = "hour",
        "86400" = "day"
      )

      station_activity <- readr::read_csv(
        I(content),
        col_types = readr::cols(
          START = readr::col_character(),
          END = readr::col_character(),
          .default = readr::col_guess()
        ),
        show_col_types = FALSE
      ) |>
        dplyr::transmute(
          station = .data$STATION,
          interval = interval_map[as.character(.data$OBS_INTERVAL)],
          interval_seconds = .data$OBS_INTERVAL,
          first_observation = as.POSIXct(
            .data$START,
            tz = ks_meso_tz()
          ),
          last_observation = as.POSIXct(
            .data$END,
            tz = ks_meso_tz()
          ),
          data_span_days = as.numeric(
            difftime(
              .data$last_observation,
              .data$first_observation,
              units = "days"
            )
          ),
          is_current = as.numeric(
            difftime(Sys.time(), .data$last_observation, units = "hours")
          ) <
            24
        ) |>
        dplyr::arrange(.data$station, .data$interval_seconds)

      # Add class for potential method dispatch
      class(station_activity) <- c("ks_meso_station_activity", "data.frame")

      return(station_activity)
    },
    error = function(e) {
      stop(sprintf("Error fetching station activity data: %s", e$message))
    }
  )
}
