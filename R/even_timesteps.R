#' Fill missing rows in an even time series
#'
#' Builds a complete timestamp sequence for logger data and joins the original
#' observations onto it. Missing timestamps become explicit rows with `NA`
#' values in the measured columns.
#'
#' @param loggerData Data frame or tibble containing timestamped logger data.
#' @param datetime_col Character string naming the POSIXct datetime column.
#'   Defaults to `"DateTime_UTC"`.
#' @param site_col Optional character string naming a site column. When
#'   supplied, each site is completed independently.
#'
#' @return A data frame or tibble, matching the input class, with all original
#'   rows plus inserted `NA` rows for missing time steps.
#'
#' @details
#' The time step is inferred from the sorted unique timestamps for each site.
#' Use this after removing obvious duplicate or invalid timestamps.
#'
#' @examples
#' df <- data.frame(
#'   DateTime_UTC = as.POSIXct(
#'     c("2024-01-01 00:00", "2024-01-01 01:00", "2024-01-01 03:00"),
#'     tz = "UTC"
#'   ),
#'   temp_C = c(10.1, 10.4, 10.5)
#' )
#'
#' even_timesteps(df)
#'
#' df_multi <- data.frame(
#'   DateTime_UTC = c(
#'     as.POSIXct(
#'       c("2024-01-01 00:00", "2024-01-01 01:00", "2024-01-01 03:00"),
#'       tz = "UTC"
#'     ),
#'     as.POSIXct(c("2024-01-01 00:00", "2024-01-01 00:30"), tz = "UTC")
#'   ),
#'   Site = c("A", "A", "A", "B", "B")
#' )
#'
#' even_timesteps(df_multi, site_col = "Site")
#'
#' @export
even_timesteps <- function(
  loggerData,
  datetime_col = "DateTime_UTC",
  site_col = NULL
) {
  # Input validation
  if (!is.data.frame(loggerData)) {
    cli::cli_abort("{.arg loggerData} must be a data frame.")
  }
  if (!datetime_col %in% names(loggerData)) {
    cli::cli_abort(
      "{.arg loggerData} must contain a {.field {datetime_col}} column."
    )
  }
  if (!is.null(site_col) && !site_col %in% names(loggerData)) {
    cli::cli_abort(
      "{.arg loggerData} must contain a {.field {site_col}} column."
    )
  }

  # Get datetime vector
  datetime_vec <- loggerData[[datetime_col]]
  if (!inherits(datetime_vec, "POSIXct")) {
    cli::cli_abort("{.field {datetime_col}} must be a POSIXct vector.")
  }

  # Function to process a single site
  process_site <- function(site_data) {
    # Find the most common time interval. Using the modal diff (rather than
    # the first one) is robust to near-duplicate timestamps and gaps that
    # would otherwise mis-infer the step.
    diffs_sec <- as.numeric(
      diff(sort(unique(site_data[[datetime_col]]))),
      units = "secs"
    )
    if (length(diffs_sec) == 0) {
      cli::cli_warn("Site has insufficient data points to determine interval.")
      return(site_data)
    }
    interval_seconds <- as.numeric(calc_mode(diffs_sec, multi = "first"))

    # Create sequence of evenly spaced timestamps
    start_time <- lubridate::ceiling_date(
      min(site_data[[datetime_col]]),
      unit = "mins"
    )
    end_time <- lubridate::floor_date(
      max(site_data[[datetime_col]]),
      unit = "mins"
    )

    # Generate even time sequence
    even_times <- seq(start_time, end_time, by = interval_seconds)

    # Create template dataframe
    template <- data.frame(temp_col = even_times)
    names(template) <- datetime_col

    # Add site column to template if needed
    if (!is.null(site_col)) {
      template[[site_col]] <- unique(site_data[[site_col]])[1]
    }

    # Merge while preserving all timestamps
    result <- merge(
      site_data,
      template,
      by = c(datetime_col, site_col),
      all = TRUE
    )

    # Sort by timestamp
    result[order(result[[datetime_col]]), , drop = FALSE]
  }

  if (is.null(site_col)) {
    # Process all data as single site
    result <- process_site(loggerData)
  } else {
    # Split by site, process each separately, and recombine
    sites <- unique(loggerData[[site_col]])
    result <- do.call(
      rbind,
      lapply(sites, function(s) {
        site_data <- loggerData[loggerData[[site_col]] == s, ]
        process_site(site_data)
      })
    )
  }

  # Reorder columns (datetime and site first)
  col_order <- unique(c(datetime_col, site_col, names(loggerData)))
  result <- result[, col_order[col_order %in% names(result)], drop = FALSE]

  # Return the same class as the input: tibble or data frame
  if (tibble::is_tibble(loggerData)) {
    result <- tibble::as_tibble(result)
  } else {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }

  result
}
