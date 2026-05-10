#' Download GHCNh CSV files
#'
#' Downloads GHCNh CSV files for specified stations and years, saving them to an
#' output directory.
#'
#' @param station_id Character string specifying the station ID for which data
#'   is to be downloaded.
#' @param years Numeric vector specifying the years for which data is to be
#'   downloaded.
#' @param output_dir Optional. Character string specifying the output directory.
#'   Defaults to NOAA cache directory.
#' @param quiet Logical. If `TRUE`, suppresses messages. Defaults to `FALSE`.
#'
#' @return A list summarizing the download process:
#'   \item{output_directory}{The directory where files were saved.}
#'   \item{successful_downloads}{Character vector of successfully downloaded files.}
#'   \item{skipped_downloads}{Character vector of files already present locally.}
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
download_ghcnh <- function(
  station_id,
  years,
  output_dir = NULL,
  quiet = FALSE
) {
  if (
    missing(station_id) ||
      !is.character(station_id) ||
      length(station_id) != 1 ||
      is.na(station_id) ||
      !nzchar(station_id)
  ) {
    cli::cli_abort("{.arg station_id} must be a single non-empty string.")
  }

  if (
    missing(years) ||
      !is.numeric(years) ||
      length(years) == 0 ||
      anyNA(years) ||
      any(!is.finite(years)) ||
      any(years != floor(years))
  ) {
    cli::cli_abort("{.arg years} must be one or more whole numeric years.")
  }
  if (
    !is.null(output_dir) &&
      (!is.character(output_dir) ||
        length(output_dir) != 1 ||
        is.na(output_dir))
  ) {
    cli::cli_abort("{.arg output_dir} must be `NULL` or a single string.")
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be `TRUE` or `FALSE`.")
  }

  # Create output directory if specified and doesn't exist
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
  } else {
    output_dir <- noaa_cache()
    if (!quiet) {
      cli::cli_inform("Saving GHCNh files to the NOAA cache directory.")
    }
  }

  # Initialize results tracking
  results <- list(
    success = character(),
    skipped = character(),
    failed = character()
  )

  # Base URL for GHCNh data
  base_url <- "https://www.ncei.noaa.gov/oa/global-historical-climatology-network/hourly/access/by-year"

  # Download files for each year
  for (year in years) {
    # Construct URL and filename
    url <- sprintf(
      "%s/%d/csv/GHCNh_%s_%d.csv.gz",
      base_url,
      year,
      station_id,
      year
    )
    output_file <- file.path(
      output_dir,
      sprintf("GHCNh_%s_%d.csv.gz", station_id, year)
    )

    if (file.exists(output_file)) {
      if (!quiet) {
        cli::cli_inform(
          "GHCNh cache file already exists for station {.val {station_id}} and year {year}; skipping download."
        )
      }
      results$skipped <- c(results$skipped, sprintf("%s_%d", station_id, year))
      next
    }

    tryCatch(
      {
        # Attempt download
        if (!quiet) {
          cli::cli_inform(
            "Downloading GHCNh data for station {.val {station_id}}, year {year}."
          )
        }
        status <- utils::download.file(
          url,
          output_file,
          mode = "wb",
          quiet = quiet
        )
        if (!isTRUE(status == 0)) {
          cli::cli_abort("Download failed with status code {status}.")
        }
        results$success <- c(
          results$success,
          sprintf("%s_%d", station_id, year)
        )
        if (!quiet) {
          cli::cli_inform(
            "Downloaded GHCNh data for station {.val {station_id}}, year {year}."
          )
        }
      },
      error = function(e) {
        unlink(output_file)
        results$failed <- c(results$failed, sprintf("%s_%d", station_id, year))
        cli::cli_warn(
          c(
            "Failed to download GHCNh data for station {.val {station_id}}, year {year}.",
            "i" = e$message
          )
        )
      }
    )
  }

  # Return results summary
  summary <- list(
    output_directory = normalizePath(output_dir),
    successful_downloads = results$success,
    skipped_downloads = results$skipped,
    failed_downloads = results$failed,
    total_attempted = length(years),
    total_successful = length(results$success),
    total_skipped = length(results$skipped)
  )

  return(summary)
}

#' Read GHCNh CSV files
#'
#' Reads GHCNh CSV files from specified files or directory and optionally
#' combines them into a single dataframe.
#'
#' @param files Optional. Character vector specifying the paths to CSV files.
#'   Defaults to `NULL`.
#' @param directory Optional. Character string specifying the directory
#'   containing CSV files. Defaults to `NULL`.
#' @param combine Logical. If `TRUE`, combines all successfully read files into
#'   a single dataframe. Defaults to `TRUE`.
#' @param quiet Logical. If `TRUE`, suppresses messages. Defaults to `FALSE`.
#'
#' @return If `combine` is `TRUE`, returns a list with:
#'   \item{data}{Combined dataframe of all successfully read files.}
#'   \item{files_read}{Character vector of successfully read files.}
#'   \item{files_failed}{Character vector of failed reads.}
#'   \item{total_rows}{Total number of rows in the combined dataframe.}
#'   \item{date_range}{Date range of the combined data.}
#'   \item{stations}{Unique stations in the combined data.}
#'   \item{removed_cols}{List of columns removed due to all NA values.}
#' If `combine` is `FALSE`, returns a list of individual dataframes and read results.
#'
#' @examples
#' \dontrun{
#'   # Read files from a directory
#'   data <- read_ghcnh(directory = "data/ghcnh")
#'
#'   # Read specific files
#'   files <- c("data/ghcnh/GHCNh_USW00023183_2022.csv.gz",
#'              "data/ghcnh/GHCNh_USW00023183_2023.csv.gz")
#'   data <- read_ghcnh(files = files)
#' }
#'
#' @importFrom rlang .data
#' @export
read_ghcnh <- function(
  files = NULL,
  directory = NULL,
  combine = TRUE,
  quiet = FALSE
) {
  # Initialize variables to avoid R CMD check notes
  DateTime <- Station_name <- Station_ID <- DATE <- Year <- Month <- Day <- Hour <- Minute <- NULL

  if (!is.null(files)) {
    if (
      !is.character(files) ||
        length(files) == 0 ||
        anyNA(files) ||
        any(!nzchar(files))
    ) {
      cli::cli_abort("{.arg files} must be `NULL` or a character vector.")
    }
  }
  if (
    !is.null(directory) &&
      (!is.character(directory) || length(directory) != 1 || is.na(directory))
  ) {
    cli::cli_abort("{.arg directory} must be `NULL` or a single string.")
  }
  if (!is.logical(combine) || length(combine) != 1 || is.na(combine)) {
    cli::cli_abort("{.arg combine} must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be `TRUE` or `FALSE`.")
  }
  if (!is.null(files) && !is.null(directory)) {
    cli::cli_abort("Use only one of {.arg files} and {.arg directory}.")
  }

  if (is.null(files) && is.null(directory)) {
    directory <- noaa_cache()
    if (!quiet) {
      cli::cli_inform("Loading GHCNh files from the NOAA cache directory.")
    }
  }

  # Find CSV files
  if (!is.null(directory)) {
    if (!dir.exists(directory)) {
      cli::cli_abort("{.arg directory} does not exist.")
    }
    files <- list.files(
      path = directory,
      pattern = "^GHCNh.*\\.csv(\\.gz)?$",
      full.names = TRUE
    )
    if (length(files) == 0) {
      cli::cli_abort("No GHCNh CSV files found in {.arg directory}.")
    }
  }
  files <- sort(files)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    cli::cli_abort(c(
      "All paths in {.arg files} must exist.",
      x = "Missing {.file {missing_files}}."
    ))
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
    code_cols <- grep(
      "(_Measurement_Code|_Report_Type|_Source_Code|_Source_Station_ID)$",
      names(df),
      value = TRUE
    )
    for (col in code_cols) {
      df[[col]] <- as.character(df[[col]])
    }

    # Numeric conversion with .data
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

    for (var in numeric_vars[numeric_vars %in% names(df)]) {
      df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
      df[[var]] <- ifelse(df[[var]] == -9999, NA_real_, df[[var]])
      if (
        var %in%
          c("temperature", "dew_point_temperature", "wet_bulb_temperature")
      ) {
        df[[var]] <- df[[var]] / 10
      }
    }

    parse_ghcnh_datetime <- function(df) {
      if ("DATE" %in% names(df)) {
        date <- df[["DATE"]]
        if (inherits(date, "POSIXt")) {
          return(as.POSIXct(date, tz = "UTC"))
        }
        datetime <- suppressWarnings(lubridate::ymd_hms(
          date,
          tz = "UTC",
          quiet = TRUE
        ))
        if (all(is.na(datetime))) {
          datetime <- suppressWarnings(lubridate::ymd_hm(
            date,
            tz = "UTC",
            quiet = TRUE
          ))
        }
        return(datetime)
      }

      if (all(c("Year", "Month", "Day", "Hour", "Minute") %in% names(df))) {
        return(as.POSIXct(
          sprintf(
            "%04d-%02d-%02d %02d:%02d:00",
            df[["Year"]],
            df[["Month"]],
            df[["Day"]],
            df[["Hour"]],
            df[["Minute"]]
          ),
          tz = "UTC"
        ))
      }

      rep(as.POSIXct(NA, tz = "UTC"), nrow(df))
    }

    datetime <- parse_ghcnh_datetime(df)

    df <- df |>
      dplyr::mutate(DateTime = datetime) |>
      dplyr::select(
        -dplyr::any_of(c("DATE", "Year", "Month", "Day", "Hour", "Minute"))
      )

    if ("Station_name" %in% names(df)) {
      df <- dplyr::relocate(df, DateTime, .after = "Station_name")
    } else {
      df <- dplyr::relocate(df, DateTime)
    }

    return(df)
  }

  # Process files
  for (file in files) {
    tryCatch(
      {
        file_info <- basename(file)
        df <- readr::read_csv(file, show_col_types = FALSE) |>
          standardize_df()
        results$data[[file_info]] <- df
        results$success <- c(results$success, file_info)
        if (!quiet) {
          cli::cli_inform("Read GHCNh file {.file {file_info}}.")
        }
      },
      error = function(e) {
        results$failed <- c(results$failed, basename(file))
        cli::cli_warn(
          c(
            "Failed to read GHCNh file {.file {basename(file)}}.",
            "i" = e$message
          )
        )
      }
    )
  }

  # Return combined or individual results
  if (combine && length(results$data) > 0) {
    combined_data <- dplyr::bind_rows(results$data)
    na_cols <- sapply(combined_data, function(x) all(is.na(x)))
    datetime <- combined_data$DateTime
    date_range <- as.Date(c(NA, NA))
    if (any(!is.na(datetime))) {
      date_range <- as.Date(range(datetime, na.rm = TRUE))
    }
    stations <- character()
    if ("Station_ID" %in% names(combined_data)) {
      stations <- unique(stats::na.omit(combined_data$Station_ID))
    }
    list(
      data = combined_data[, !na_cols],
      files_read = results$success,
      files_failed = results$failed,
      total_rows = nrow(combined_data),
      date_range = date_range,
      stations = stations,
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
