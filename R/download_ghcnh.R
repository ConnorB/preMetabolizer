#' Download GHCNh Parquet Files
#'
#' Downloads GHCNh parquet files for specified stations and years, saving them to an output directory.
#'
#' @param station_id Character string specifying the station ID for which data is to be downloaded.
#' @param years Numeric vector specifying the years for which data is to be downloaded.
#' @param output_dir Optional. Character string specifying the output directory. Defaults to NOAA cache directory.
#' @param quiet Logical. If \code{TRUE}, suppresses messages. Defaults to \code{FALSE}.
#'
#' @return A list summarizing the download process:
#'   \item{output_directory}{The directory where files were saved.}
#'   \item{successful_downloads}{Character vector of successfully downloaded files.}
#'   \item{failed_downloads}{Character vector of failed downloads.}
#'   \item{total_attempted}{Total number of download attempts.}
#'   \item{total_successful}{Total number of successful downloads.}
#'
#' @examples
#' \dontrun{
#'   download_ghcnh("USW00023183", 2020:2022, output_dir = "data/ghcnh")
#' }
#'
#' @export
#' @importFrom utils download.file
download_ghcnh <- function(station_id, years, output_dir = NULL, quiet = FALSE) {
  # Input validation
  if (missing(station_id) || !is.character(station_id)) {
    stop("station_id must be provided as a character string")
  }

  if (missing(years) || !is.numeric(years)) {
    stop("years must be provided as numeric values")
  }

  # Create output directory if specified and doesn't exist
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  } else {
    output_dir <- noaa_cache()
    message("Saving to cache directory")
  }

  # Initialize results tracking
  results <- list(
    success = character(),
    failed = character()
  )

  # Base URL for GHCNh data
  base_url <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"

  # Download files for each year
  for (year in years) {
    # Construct URL and filename
    url <- sprintf("%s/%d/parquet/GHCNh_%s_%d.parquet", base_url, year, station_id, year)
    output_file <- file.path(output_dir, sprintf("GHCNh_%s_%d.parquet", station_id, year))

    if (file.exists(output_file)) {
      if(!quiet) message(paste0(sprintf("Cache file exist for station %s and year %d", station_id, year), ", skipping download"))
      next
    }

    tryCatch({
      # Attempt download
      if(!quiet) message(sprintf("Downloading data for station %s, year %d", station_id, year))
      download.file(url, output_file, mode = "auto", quiet = T)
      results$success <- c(results$success, sprintf("%s_%d", station_id, year))
      message(sprintf("Successfully downloaded data for station %s, year %d", station_id, year))
    }, error = function(e) {
      results$failed <- c(results$failed, sprintf("%s_%d", station_id, year))
      warning(sprintf("Failed to download data for station %s, year %d: %s",
                      station_id, year, e$message))
    })
  }

  # Return results summary
  summary <- list(
    output_directory = normalizePath(output_dir),
    successful_downloads = results$success,
    failed_downloads = results$failed,
    total_attempted = length(years),
    total_successful = length(results$success)
  )

  return(summary)
}

#' Read GHCNh Parquet Files
#'
#' Reads GHCNh parquet files from specified files or directory and optionally combines them into a single dataframe.
#'
#' @param files Optional. Character vector specifying the paths to parquet files. Defaults to \code{NULL}.
#' @param directory Optional. Character string specifying the directory containing parquet files. Defaults to \code{NULL}.
#' @param combine Logical. If \code{TRUE}, combines all successfully read files into a single dataframe. Defaults to \code{TRUE}.
#'
#' @return If \code{combine} is \code{TRUE}, returns a list with:
#'   \item{data}{Combined dataframe of all successfully read files.}
#'   \item{files_read}{Character vector of successfully read files.}
#'   \item{files_failed}{Character vector of failed reads.}
#'   \item{total_rows}{Total number of rows in the combined dataframe.}
#'   \item{date_range}{Date range of the combined data.}
#'   \item{stations}{Unique stations in the combined data.}
#'   \item{removed_cols}{List of columns removed due to all NA values.}
#' If \code{combine} is \code{FALSE}, returns a list of individual dataframes and read results.
#'
#' @examples
#' \dontrun{
#'   # Read files from a directory
#'   data <- read_ghcnh(directory = "data/ghcnh")
#'
#'   # Read specific files
#'   files <- c("data/ghcnh/GHCNh_USW00023183_2022.parquet",
#'              "data/ghcnh/GHCNh_USW00023183_2023.parquet")
#'   data <- read_ghcnh(files = files)
#' }
#'
#' @importFrom dplyr mutate select relocate any_of bind_rows
#' @importFrom arrow read_parquet
#' @importFrom rlang .data
read_ghcnh <- function(files = NULL, directory = NULL, combine = TRUE) {
  # Initialize variables to avoid R CMD check notes
  DateTime <- Station_name <- Station_ID <- DATE <- Year <- Month <- Day <- Hour <- Minute <- NULL

  # Input validation
  if (is.null(files) && is.null(directory)) {
    directory <- noaa_cache()
    message("Loading files from cache")
  }

  # Find parquet files
  if (!is.null(directory)) {
    files <- list.files(
      path = directory,
      pattern = "^GHCNh.*\\.parquet$",
      full.names = TRUE
    )
    if (length(files) == 0) {
      stop("No GHCNh parquet files found in directory")
    }
  }

  # Initialize results
  results <- list(
    data = list(),
    success = character(),
    failed = character()
  )

  # Quality code handler
  handle_quality_code <- function(x) {
    if (is.data.frame(x)) as.character(x$member0) else as.character(x)
  }

  # Standardization function using .data pronoun
  standardize_df <- function(df) {
    # Handle quality codes
    quality_cols <- grep("_Quality_Code$", names(df), value = TRUE)
    for (col in quality_cols) {
      df[[col]] <- handle_quality_code(df[[col]])
    }

    # Convert code columns
    code_cols <- grep("(_Measurement_Code|_Report_Type|_Source_Code|_Source_Station_ID)$",
                      names(df), value = TRUE)
    for (col in code_cols) {
      df[[col]] <- as.character(df[[col]])
    }

    # Numeric conversion with .data
    numeric_vars <- c(
      "temperature", "dew_point_temperature", "station_level_pressure",
      "sea_level_pressure", "wind_direction", "wind_speed", "wind_gust",
      "precipitation", "relative_humidity", "wet_bulb_temperature",
      "snow_depth", "visibility", "altimeter", "pressure_3hr_change",
      paste0("precipitation_", c(3,6,9,12,15,18,21,24), "_hour")
    )

    for (var in numeric_vars[numeric_vars %in% names(df)]) {
      df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
      df[[var]] <- ifelse(df[[var]] == -9999, NA_real_, df[[var]])
      if (var %in% c("temperature", "dew_point_temperature", "wet_bulb_temperature")) {
        df[[var]] <- df[[var]] / 10
      }
    }

    # DateTime handling with .data
    df <- df |>
      dplyr::mutate(
        DateTime = if ("DATE" %in% names(.data)) {
          as.POSIXct(.data$DATE, tz = "UTC", format = "%Y-%m-%dT%H:%M:%S")
        } else if (all(c("Year", "Month", "Day", "Hour", "Minute") %in% names(.data))) {
          as.POSIXct(
            sprintf("%04d-%02d-%02d %02d:%02d:00",
                    .data$Year, .data$Month, .data$Day, .data$Hour, .data$Minute),
            tz = "UTC"
          )
        } else {
          NA_real_
        }
      ) |>
      dplyr::select(-dplyr::any_of(c("DATE", "Year", "Month", "Day", "Hour", "Minute"))) |>
      dplyr::relocate(DateTime, .after = .data$Station_name)

    return(df)
  }

  # Process files
  for (file in files) {
    tryCatch({
      file_info <- basename(file)
      df <- arrow::read_parquet(file) |>
        standardize_df()
      results$data[[file_info]] <- df
      results$success <- c(results$success, file_info)
      message("Successfully read: ", file_info)
    }, error = function(e) {
      results$failed <- c(results$failed, basename(file))
      warning("Failed to read ", basename(file), ": ", e$message)
    })
  }

  # Return combined or individual results
  if (combine && length(results$data) > 0) {
    combined_data <- dplyr::bind_rows(results$data)
    na_cols <- sapply(combined_data, function(x) all(is.na(x)))
    list(
      data = combined_data[, !na_cols],
      files_read = results$success,
      files_failed = results$failed,
      total_rows = nrow(combined_data),
      date_range = as.Date(range(combined_data$DateTime, na.rm = TRUE)),
      stations = unique(combined_data$Station_ID),
      removed_cols = names(na_cols)[na_cols]
    )
  } else {
    list(
      data = results$data,
      files_read = results$success,
      files_failed = results$failed
    )
  }
}
