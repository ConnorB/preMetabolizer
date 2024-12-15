#' Get evenly spaced time steps for logger data
#' @description Creates a dataframe with evenly spaced time steps by merging original
#'   data with a sequence of timestamps at the most common sampling interval. Can handle
#'   multiple sites with different time intervals.
#'
#' @param loggerData A data frame containing a DateTime_UTC column with timestamp data
#' @param datetime_col Character string specifying the datetime column name.
#'   Defaults to "DateTime_UTC"
#' @param site_col Character string specifying the site column name. If NULL (default),
#'   treats all data as a single site.
#'
#' @return A data frame with evenly spaced time steps for each site, including all original data
#'   points plus NAs for missing intervals
#' @import lubridate
#' @importFrom dplyr group_by ungroup do bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' # Single site usage
#' df <- data.frame(
#'   DateTime_UTC = seq(as.POSIXct("2024-01-01"), by = "1 hour", length.out = 10)
#' )
#' even_timesteps(df)
#'
#' # Multiple sites
#' df_multi <- data.frame(
#'   DateTime_UTC = c(
#'     seq(as.POSIXct("2024-01-01"), by = "1 hour", length.out = 5),
#'     seq(as.POSIXct("2024-01-01"), by = "30 min", length.out = 5)
#'   ),
#'   Site = c(rep("Site1", 5), rep("Site2", 5))
#' )
#' even_timesteps(df_multi, site_col = "Site")
#' }
even_timesteps <- function(loggerData, datetime_col = "DateTime_UTC", site_col = NULL) {
  # Input validation
  if (!is.data.frame(loggerData)) {
    stop("Input must be a data frame")
  }
  if (!datetime_col %in% names(loggerData)) {
    stop(sprintf("Column '%s' not found in input data frame", datetime_col))
  }
  if (!is.null(site_col) && !site_col %in% names(loggerData)) {
    stop(sprintf("Site column '%s' not found in input data frame", site_col))
  }

  # Get datetime vector
  datetime_vec <- loggerData[[datetime_col]]
  if (!inherits(datetime_vec, "POSIXct")) {
    stop(sprintf("Column '%s' must be of class POSIXct", datetime_col))
  }

  # Function to process a single site
  process_site <- function(site_data) {
    # Find most common time interval for this site
    time_diffs <- diff(sort(unique(site_data[[datetime_col]])))[1] # Use first diff as representative
    if (length(time_diffs) == 0) {
      warning("Site has insufficient data points to determine interval")
      return(site_data)
    }

    # Convert to seconds for sequence generation
    interval_seconds <- as.numeric(time_diffs, unit = "secs")

    # Create sequence of evenly spaced timestamps
    start_time <- ceiling_date(min(site_data[[datetime_col]]), unit = "mins")
    end_time <- floor_date(max(site_data[[datetime_col]]), unit = "mins")

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
    result <- merge(site_data, template, by = c(datetime_col, site_col), all = TRUE)

    # Sort by timestamp
    result[order(result[[datetime_col]]), ]
  }

  if (is.null(site_col)) {
    # Process all data as single site
    result <- process_site(loggerData)
  } else {
    # Split by site, process each separately, and recombine
    sites <- unique(loggerData[[site_col]])
    result <- do.call(rbind, lapply(sites, function(s) {
      site_data <- loggerData[loggerData[[site_col]] == s, ]
      process_site(site_data)
    }))
  }

  # Preserve original column order while ensuring datetime and site columns come first
  col_order <- unique(c(datetime_col, site_col, names(loggerData)))
  result[, col_order[col_order %in% names(result)]]
}

#' Find the mode (most frequent value) in a vector
#' @param x A vector of values
#' @return The most frequent value(s) in the vector
#' @keywords internal
find_mode <- function(x) {
  if (length(x) == 0) {
    return(numeric(0))
  }

  # Handle ties by returning all modes
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
