#' Fetch Data from Kansas Mesonet
#'
#' Retrieves weather data for specified stations from the Kansas Mesonet.
#'
#' @param stations Character vector of station names to retrieve data for.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` format.
#' @param interval Data interval. Must be one of `'hour'`, `'5min'`, or `'day'`.
#' @param vars Character vector of variables to retrieve. Defaults to common variables.
#' @param output_dir Directory to save the downloaded data. Defaults to the cache path.
#' @param debug Logical; if `TRUE`, debug messages are printed.
#'
#' @return A list with details about successful and failed downloads, output directory, and data chunks.
#'
#' @importFrom arrow write_parquet
#' @importFrom utils read.csv
#' @importFrom utils URLencode
#' @importFrom rlang .data
#'
#' @export
get_ks_meso <- function(stations, start_date, end_date, interval, vars = NULL,
                        output_dir = NULL, debug = T) {
  # Input validation with better error messages and type checking
  stopifnot(
    "stations must be a character vector" = is.character(stations) && length(stations) > 0,
    "interval must be one of: 'hour', '5min', 'day'" = interval %in% c("hour", "5min", "day")
  )

  # Validate dates using tryCatch for better error handling
  dates <- tryCatch({
    list(
      start = as.Date(start_date),
      end = as.Date(end_date)
    )
  }, error = function(e) {
    stop("start_date and end_date must be valid dates in YYYY-MM-DD format")
  })

  if (dates$start > dates$end) {
    stop("start_date must be before or equal to end_date")
  }

  # Use default variables if none provided
  vars <- if (is.null(vars)) {
    c("PRESSUREAVG", "TEMP2MAVG", "TEMP2MMIN", "TEMP2MMAX",
      "RELHUM2MAVG", "PRECIP")
  } else {
    vars
  }

  # Validate stations against available stations
  available_stations <- ks_meso_stations()$StationName
  invalid_stations <- setdiff(stations, available_stations)
  if (length(invalid_stations) > 0) {
    stop(sprintf("Invalid stations: %s", paste(invalid_stations, collapse = ", ")))
  }

  # Validate stations against available stations
  available_vars <- preMetabolizer::ks_mesonet_vars$Variable
  invalid_vars <- setdiff(vars, available_vars)
  if (length(invalid_vars) > 0) {
    stop(sprintf("Invalid variables: %s", paste(invalid_stations, collapse = ", ")))
  }

  # Handle output directory
  output_dir <- if (is.null(output_dir)) {
    path <- mesonet_cache()
    if (debug) message("Using cache directory: ", path)
    path
  } else {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_dir
  }
  # Calculate records per interval and max days per call
  records_per_interval <- list(
    hour = 24,    # 24 records per day
    "5min" = 288, # 288 records per day (12 per hour * 24 hours)
    day = 1       # 1 record per day
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

  # Process each station sequentially
  for (station in stations) {
    if (debug) message(sprintf("Processing station: %s", station))

    tryCatch({
      output_file <- file.path(output_dir,
                               sprintf("Mesonet_%s_%s_%s_to_%s.parquet",
                                       station, interval, start_date, end_date))

      # Skip if file exists
      if (file.exists(output_file)) {
        if (debug) message(sprintf("File exists: %s", output_file))
        results$success <- c(results$success, station)
        next
      }

      # Generate date chunks with smaller intervals
      date_sequence <- seq(dates$start, dates$end, by = "days")
      chunks <- split(date_sequence, ceiling(seq_along(date_sequence) / max_days_per_call))

      # Initialize list to store chunk data
      chunk_data <- vector("list", length(chunks))
      chunk_success <- logical(length(chunks))

      # Download data chunks
      for (i in seq_along(chunks)) {
        chunk <- chunks[[i]]
        time_start <- format(min(chunk), "%Y%m%d000000")
        time_end <- format(max(chunk), "%Y%m%d235959")

        api_url <- sprintf(
          "http://mesonet.k-state.edu/rest/stationdata/?stn=%s&int=%s&t_start=%s&t_end=%s&vars=%s",
          URLencode(station, reserved = TRUE),
          interval,
          time_start,
          time_end,
          paste(vars, collapse = ",")
        )

        if (debug) message(sprintf("Downloading %s: %s to %s",
                                   station, time_start, time_end))

        # Try to download with automatic retry and error handling
        response <- tryCatch({
          resp <- httr::RETRY("GET", api_url, times = 3)
          httr::stop_for_status(resp)
          resp
        }, error = function(e) {
          if (debug) message(sprintf("Error in chunk %d: %s", i, e$message))
          if (grepl("limit of 3000 records", e$message)) {
            max_days_per_call <<- floor(max_days_per_call * 0.75)
            if (debug) message(sprintf("Reducing max_days_per_call to %d", max_days_per_call))
          }
          return(NULL)
        })

        if (!is.null(response)) {
          tryCatch({
            # Read data and convert timestamp
            temp_data <- data.table::fread(
              text = httr::content(response, "text", encoding = "UTF-8"),
              na.strings = c("", "NA", "M"),
              tz = "",
              data.table = FALSE
            )

            # Convert to tibble and use .data$ pronoun for timestamp conversion
            temp_data <- dplyr::as_tibble(temp_data) |>
              dplyr::mutate(TIMESTAMP = as.POSIXct(.data$TIMESTAMP, tz = "Etc/GMT-6"))

            chunk_data[[i]] <- temp_data
            chunk_success[i] <- TRUE

            results$chunks[[length(results$chunks) + 1]] <- list(
              station = station,
              start_time = time_start,
              end_time = time_end,
              url = api_url,
              records = nrow(chunk_data[[i]])
            )
          }, error = function(e) {
            if (debug) message(sprintf("Error parsing chunk %d: %s", i, e$message))
            chunk_success[i] <- FALSE
          })
        }

        Sys.sleep(0.5)  # Rate limiting
      }

      # Remove failed chunks
      chunk_data <- chunk_data[chunk_success]

      # Combine chunks and write to Parquet
      if (length(chunk_data) > 0 && any(sapply(chunk_data, nrow) > 0)) {
        # Use dplyr's bind_rows instead of data.table::rbindlist
        combined_data <- dplyr::bind_rows(chunk_data)

        # Write to Parquet with compression
        arrow::write_parquet(
          combined_data,
          output_file,
          compression = "snappy"
        )

        results$success <- c(results$success, station)

        if (debug) message(sprintf("Data saved to: %s", output_file))
      } else {
        warning(sprintf("No data retrieved for station %s", station))
        results$failed <- c(results$failed, station)
      }

    }, error = function(e) {
      message(sprintf("Error processing station %s: %s", station, e$message))
      results$failed <- c(results$failed, station)
    })
  }

  # Return summary
  list(
    output_directory = normalizePath(output_dir),
    successful_downloads = results$success,
    failed_downloads = results$failed,
    total_attempted = length(stations),
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
#' @importFrom arrow read_parquet
#' @export
read_ks_meso <- function(station, start_date, end_date, interval, output_dir = NULL) {
  if (missing(station) || !is.character(station)) {
    stop("station must be provided as a character string")
  }

  if (missing(start_date) || missing(end_date) || !is.Date(ymd(start_date)) || !is.Date(ymd(end_date))) {
    stop("start_date and end_date must be provided as valid date strings (YYYY-MM-DD)")
  }

  if (missing(interval) || !is.character(interval)) {
    stop("interval must be provided as a character string")
  }

  if (is.null(output_dir)) {
    output_dir <- mesonet_cache()
  }

  # Create the expected file path
  output_file <- file.path(output_dir, sprintf("Mesonet_%s_%s_%s_to_%s.parquet", station, interval, start_date, end_date))

  if (!file.exists(output_file)) {
    stop(sprintf("Cached file for station %s, interval %s, and date range %s to %s does not exist.",
                 station, interval, start_date, end_date))
  }

  # Read and return the cached Parquet data
  data <- arrow::read_parquet(output_file)
  return(data)
}

#' Get Kansas Mesonet Station Information
#'
#' Fetches metadata about Kansas Mesonet stations, including location and operator details.
#'
#' @return A data frame containing station metadata.
#'
#' @importFrom utils read.csv
#' @export
ks_meso_stations <- function() {
  url <- "http://mesonet.k-state.edu/rest/stationnames/"
  col_names <- c("StationName", "County", "Latitude", "Longitude", "Elevation_m", "Network", "Abbreviation", "OperatorName", "FW13")
  tryCatch({
    response <- httr::GET(url)
    if (httr::status_code(response) != 200) {
      stop("Failed to fetch station names")
    }
    content <- httr::content(response, as = "text")
    station_names <- read.csv(text = content, header = T, col.names = col_names)
    return(station_names)
  }, error = function(e) {
    stop(sprintf("Error fetching station names: %s", e$message))
  })
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

  tryCatch({
    # Fetch data with retry capability
    response <- httr::RETRY(
      "GET",
      url,
      times = 3,
      quiet = TRUE
    )

    httr::stop_for_status(response)
    content <- httr::content(response, as = "text", encoding = "UTF-8")

    # Parse the CSV data using data.table - note that headers are present
    station_activity <- data.table::fread(
      text = content,
      header = TRUE,
      data.table = FALSE,
      stringsAsFactors = FALSE
    )

    # Convert interval seconds to more readable format
    interval_map <- c(
      "300" = "5min",
      "3600" = "hour",
      "86400" = "day"
    )

    station_activity$interval <- interval_map[as.character(station_activity$OBS_INTERVAL)]

    # Convert timestamps - they're already in the correct format
    station_activity$START <- as.POSIXct(station_activity$START, tz = "America/Chicago")
    station_activity$END <- as.POSIXct(station_activity$END, tz = "America/Chicago")

    # Add additional computed columns
    station_activity$data_span_days <- as.numeric(
      difftime(station_activity$END,
               station_activity$START,
               units = "days")
    )

    station_activity$is_current <- as.numeric(
      difftime(Sys.time(),
               station_activity$END,
               units = "hours")
    ) < 24  # Consider current if updated in last 24 hours

    # Rename and reorder columns for consistency
    names(station_activity)[names(station_activity) == "STATION"] <- "station"
    names(station_activity)[names(station_activity) == "OBS_INTERVAL"] <- "interval_seconds"
    names(station_activity)[names(station_activity) == "START"] <- "first_observation"
    names(station_activity)[names(station_activity) == "END"] <- "last_observation"

    # Reorder columns
    station_activity <- station_activity[, c(
      "station",
      "interval",
      "interval_seconds",
      "first_observation",
      "last_observation",
      "data_span_days",
      "is_current"
    )]

    # Sort by station name and interval
    station_activity <- station_activity[order(station_activity$station,
                                               station_activity$interval_seconds), ]

    # Add class for potential method dispatch
    class(station_activity) <- c("ks_meso_station_activity", "data.frame")

    return(station_activity)

  }, error = function(e) {
    stop(sprintf("Error fetching station activity data: %s", e$message))
  })
}
