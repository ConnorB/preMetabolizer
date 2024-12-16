#' Validate input data structure
#' @param df Input dataframe
#' @param metadata Metadata dataframe
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_input_data <- function(df, metadata) {
  # Check if dataframes are empty
  if (nrow(df) == 0) {
    stop("Input dataframe is empty")
  }
  if (nrow(metadata) == 0) {
    stop("Metadata dataframe is empty")
  }

  # Check if required columns exist in df
  if (!"Site" %in% names(df)) {
    stop("Input dataframe must contain a 'Site' column")
  }
  if (!"dateTime" %in% names(df)) {
    stop("Input dataframe must contain a 'dateTime' column")
  }

  # Check if required columns exist in metadata
  required_meta_cols <- c("Site", "Lat", "Long", "Elev_m")
  missing_meta_cols <- setdiff(required_meta_cols, names(metadata))
  if (length(missing_meta_cols) > 0) {
    stop(
      "Missing required metadata columns: ",
      paste(missing_meta_cols, collapse = ", ")
    )
  }

  # Check data types
  if (!inherits(df$dateTime, c("POSIXct", "POSIXt"))) {
    stop("dateTime column must be POSIXct")
  }

  # Validate metadata values
  if (!all(metadata$Lat >= -90 & metadata$Lat <= 90)) {
    stop("Invalid latitude values in metadata")
  }
  if (!all(metadata$Long >= -180 & metadata$Long <= 180)) {
    stop("Invalid longitude values in metadata")
  }

  # Check if all sites in df exist in metadata
  missing_sites <- setdiff(unique(df$Site), metadata$Site)
  if (length(missing_sites) > 0) {
    stop(
      "The following sites are missing from metadata: ",
      paste(missing_sites, collapse = ", ")
    )
  }

  TRUE
}

#' Validate imputation parameters
#' @param method Imputation method
#' @param maxgap Maximum gap size
#' @return TRUE if valid, throws error otherwise
#' @keywords internal
validate_parameters <- function(method, maxgap) {
  # Validate method
  valid_methods <- c("kalman", "ma", "linear", "spline")
  if (!method %in% valid_methods) {
    stop(
      "Invalid imputation method. Must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # Validate maxgap
  if (!is.numeric(maxgap) || maxgap < 0) {
    stop("maxgap must be a non-negative number")
  }

  TRUE
}

#' Split dataframe by site
#' @param df Dataframe to split
#' @return List of dataframes split by site
#' @keywords internal
#' @importFrom rlang .data
split_by_site <- function(df) {
  # Validate required columns
  required_columns <- c("Site")

  if (!all(required_columns %in% colnames(df))) {
    stop(
      "Missing required columns in metadata: ",
      paste(setdiff(required_columns, colnames(df)), collapse = ", ")
    )
  }
  df |>
    dplyr::group_by(.data$Site) |>
    dplyr::group_split()
}

#' Get site-specific metadata
#' @param site_name Name of the site
#' @param metadata Full metadata dataframe
#' @return Site-specific metadata
#' @keywords internal
#' @importFrom rlang .data
get_site_metadata <- function(site_name, metadata) {
  # Validate required columns
  required_columns <- c("Site", "Lat", "Long")

  if (!all(required_columns %in% colnames(metadata))) {
    stop(
      "Missing required columns in metadata: ",
      paste(setdiff(
        required_columns, colnames(metadata)
      ), collapse = ", ")
    )
  }

  # Filter metadata for the specific site
  site_meta <- metadata |>
    dplyr::filter(.data$Site == site_name)

  # Validate site presence
  if (nrow(site_meta) == 0) {
    stop(sprintf("Site '%s' not found in metadata", site_name))
  }

  # Validate latitude and longitude
  if (any(is.na(site_meta[c("Lat", "Long")]))) {
    stop(sprintf("Latitude or Longitude missing for site: %s", site_name))
  }

  site_meta
}


#' Impute values for a single site
#' @param site_df Single site dataframe
#' @param metadata Site metadata
#' @param method Imputation method
#' @param maxgap Maximum gap size to fill
#' @return Imputed dataframe for single site
#' @keywords internal
impute_single_site <- function(site_df, metadata, method, maxgap) {
  site_name <- unique(site_df$Site)
  site_meta <- get_site_metadata(site_name, metadata)

  message(sprintf("\nSite %s:", site_name))

  # Impute numeric columns
  site_df <- impute_numeric_columns(site_df, method, maxgap)

  # Handle PAR data if present
  if ("PAR.obs" %in% colnames(site_df)) {
    site_df <- gap_fill_par(site_df, site_meta)
  }

  site_df
}

#' Impute numeric columns using specified method
#' @param df Dataframe containing numeric columns
#' @param method Imputation method
#' @param maxgap Maximum gap size to fill
#' @return Dataframe with imputed numeric columns
#' @keywords internal
impute_numeric_columns <- function(df, method, maxgap) {
  numeric_cols <- setdiff(names(df)[sapply(df, is.numeric)], "PAR.obs")

  for (col in numeric_cols) {
    df <- impute_numeric_column(df, col, method, maxgap)
  }

  df
}

#' Impute a single numeric column
#' @param df Dataframe containing the column
#' @param col Column name
#' @param method Imputation method
#' @param maxgap Maximum gap size to fill
#' @return Dataframe with imputed column
#' @keywords internal
impute_numeric_column <- function(df, col, method, maxgap) {
  na_count <- sum(is.na(df[[col]]))
  message(sprintf("  %s: %d gaps (filling with %s)", col, na_count, method))

  tryCatch(
    {
      df[[col]] <- apply_imputation_method(df[[col]], method, maxgap)
    },
    error = function(e) {
      message(sprintf("  Error processing %s: %s", col, e$message))
    }
  )

  df
}

#' Gap Fill PAR (light) data
#' @param df Dataframe containing PAR data
#' @param site_meta Site metadata
#' @return Dataframe with gaps in PAR values filled with calc_light() from streamMetabolizer
#' @keywords internal
gap_fill_par <- function(df, site_meta) {
  tryCatch(
    {
      na_count <- sum(is.na(df$PAR.obs))
      message(sprintf("  PAR.obs: %d gaps (filling with modeled light)", na_count))

      df <- calculate_par(df, site_meta)
      df <- clean_par(df)
    },
    error = function(e) {
      message(sprintf("  Error processing PAR.obs: %s", e$message))
    }
  )

  df
}
#' Calculate PAR with streamMetabolizer
#' @param df Dataframe containing datetime data
#' @param site_meta Site metadata
#' @return Dataframe with calculated solar metrics
#' @keywords internal
#' @importFrom streamMetabolizer convert_UTC_to_solartime calc_light
calculate_par <- function(df, site_meta) {
  df$solar.time <- streamMetabolizer::convert_UTC_to_solartime(df$dateTime,
    longitude = site_meta$Long,
    time.type = "mean solar"
  )

  maxLight <- max(df$PAR.obs, na.rm = TRUE)

  df$CalcLight <- streamMetabolizer::calc_light(
    df$solar.time,
    latitude = site_meta$Lat,
    longitude = site_meta$Long,
    max.PAR = maxLight
  )

  df
}

#' Clean and merge PAR values
#' @param df Dataframe containing PAR data
#' @return Cleaned dataframe
#' @keywords internal
#' @importFrom dplyr coalesce
clean_par <- function(df) {
  df$PAR.obs[df$PAR.obs < 0.01] <- 0
  df$PAR.obs <- dplyr::coalesce(df$PAR.obs, df$CalcLight)
  df$CalcLight <- NULL
  df$solar.time <- NULL
  df
}
