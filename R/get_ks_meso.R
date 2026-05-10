#' Fetch data from Kansas Mesonet
#'
#' Retrieves weather data for specified stations from the Kansas Mesonet.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param stations Character vector of station names to retrieve data for. Use
#'   `"all"` to retrieve data for all stations. Must be `NULL` when `network`
#'   is supplied.
#' @param network Network name to retrieve data for, one of:
#'   - `"BBW"`: Big Bend Groundwater Management District
#'   - `"EBW"`: Equus Beds Groundwater Management District
#'   - `"KSRE"`: K-State Research and Extension
#'
#'   This is an alternative to `stations`.
#' @param start_date Start date for the data retrieval in `YYYY-MM-DD` format.
#' @param end_date End date for the data retrieval in `YYYY-MM-DD` format.
#' @param interval Data interval. Must be one of `"hour"`, `"5min"`, or
#'   `"day"`.
#' @param vars Character vector of variables to retrieve. Defaults to common variables.
#' @param output_dir Directory to save the downloaded data. Defaults to the cache path.
#' @param debug Logical; if `TRUE`, debug messages are printed.
#'
#' @return A list with details about successful and failed downloads, output
#'   directory, and data chunks.
#'
#' @details
#' Kansas Mesonet data are preliminary and subject to revision. Cite the Kansas
#' Mesonet when sharing, publishing, or otherwise disseminating data accessed
#' with this function. A suggested citation format is: Kansas Mesonet, year:
#' webpage title. Accessed date, webpage URL. Review the Kansas Mesonet data
#' usage policy before automated use; automated page scraping or data ingesting
#' without written consent is not permitted.
#'
#' @references
#' Kansas Mesonet data usage policy:
#' \url{https://mesonet.k-state.edu/about/usage/}
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_ks_meso(
#'   stations = "Konza Prairie",
#'   start_date = "2024-06-01",
#'   end_date = "2024-06-07",
#'   interval = "hour",
#'   vars = c("TEMP2MAVG", "RELHUM2MAVG", "PRESSUREAVG")
#' )
#' }
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
  check_bool(debug)
  if (!is.null(stations) && !is.null(network)) {
    cli::cli_abort("Use only one of {.arg stations} and {.arg network}.")
  }
  if (is.null(stations) && is.null(network)) {
    cli::cli_abort("One of {.arg stations} or {.arg network} must be supplied.")
  }
  if (
    !is.null(stations) && (!is.character(stations) || length(stations) == 0)
  ) {
    cli::cli_abort("{.arg stations} must be a character vector.")
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
      cli::cli_abort("Invalid station{?s}: {.val {invalid_stations}}.")
    }
  }

  # Validate variables against available variables
  available_vars <- ks_meso_vars()$csv_heading
  invalid_vars <- setdiff(vars, available_vars)
  if (length(invalid_vars) > 0) {
    cli::cli_abort("Invalid variable{?s}: {.val {invalid_vars}}.")
  }

  # Handle output directory
  output_dir <- if (is.null(output_dir)) {
    path <- mesonet_cache()
    if (debug) {
      cli::cli_inform("Using Kansas Mesonet cache directory {.path {path}}.")
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
      cli::cli_inform("Processing {query_type} {.val {request_name}}.")
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
            cli::cli_inform(
              "Using cached Kansas Mesonet file {.path {output_file}}."
            )
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
        chunk_requests <- vector("list", length(chunks))
        chunk_info <- vector("list", length(chunks))

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

          if (debug) {
            cli::cli_inform(
              "Downloading {.val {request_name}} from {time_start} to {time_end}."
            )
          }

          chunk_requests[[i]] <- request
          chunk_info[[i]] <- list(
            start_time = time_start,
            end_time = time_end,
            url = request$url
          )
        }

        responses <- http_req_perform_parallel(
          chunk_requests,
          on_error = "continue"
        )

        for (i in seq_along(responses)) {
          response <- responses[[i]]

          if (inherits(response, "error")) {
            if (debug) {
              cli::cli_inform(
                c(
                  "!" = "Kansas Mesonet chunk {i} failed.",
                  "i" = conditionMessage(response)
                )
              )
            }
          } else {
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
                  start_time = chunk_info[[i]]$start_time,
                  end_time = chunk_info[[i]]$end_time,
                  url = chunk_info[[i]]$url,
                  records = nrow(chunk_data[[i]])
                )
              },
              error = function(e) {
                if (debug) {
                  cli::cli_inform(
                    c(
                      "!" = "Could not parse Kansas Mesonet chunk {i}.",
                      "i" = e$message
                    )
                  )
                }
                chunk_success[i] <- FALSE
              }
            )
          }
        }

        # Remove failed chunks
        chunk_data <- chunk_data[chunk_success]

        # Combine chunks and write to CSV
        if (length(chunk_data) > 0 && any(sapply(chunk_data, nrow) > 0)) {
          combined_data <- dplyr::bind_rows(chunk_data)
          output_data <- format_ks_meso_for_cache(combined_data)
          readr::write_csv(output_data, output_file)

          results$success <- c(results$success, request_name)

          if (debug) {
            cli::cli_inform(
              "Saved Kansas Mesonet data to {.path {output_file}}."
            )
          }
        } else {
          cli::cli_warn(
            "No data retrieved for {query_type} {.val {request_name}}."
          )
          results$failed <- c(results$failed, request_name)
        }
      },
      error = function(e) {
        cli::cli_inform(
          c(
            "!" = "Error processing {query_type} {.val {request_name}}.",
            "i" = e$message
          )
        )
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
