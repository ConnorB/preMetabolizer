#' Rolling Z-Score Calculation
#'
#' Computes rolling z-scores for a numeric vector, identifying outliers based on a specified window size.
#'
#' @param x A numeric vector.
#' @param window_size An integer specifying the size of the rolling window (default is 30).
#'
#' @return A vector of integers (0 or 1) indicating whether each value in `x` is an outlier (1) or not (0).
#'
#' @keywords internal
flag_outlier <- function(x, window_size = 30) {
  # Input validation
  if (!is.numeric(x)) stop("Input must be a numeric vector")
  if (window_size < 2) stop("Window size must be at least 2")

  # Pre-allocate vectors for speed
  n <- length(x)
  roll_mean <- roll_sd <- rep(NA_real_, n)
  half_window <- floor(window_size/2)

  # Calculate once outside the loop
  window_size_minus_1 <- window_size - 1

  # Main calculation loop
  for(i in (half_window + 1):(n - half_window)) {
    # Get window indices excluding center point
    window_idx <- (i - half_window):(i + half_window)
    window_idx <- window_idx[window_idx != i]

    # Calculate stats excluding middle value
    window_vals <- x[window_idx]
    roll_mean[i] <- mean(window_vals, na.rm = TRUE)
    roll_sd[i] <- sd(window_vals, na.rm = TRUE)
  }

  # Vectorized z-score calculation
  zscore <- (x - roll_mean) / roll_sd

  # Vectorized flag creation
  as.integer(abs(zscore) > 3)
}

#' Rolling Z-Score for Multiple Columns
#'
#' Applies the rolling z-score calculation to multiple columns in a dataframe.
#'
#' @param data A dataframe or list containing numeric columns.
#' @param window_size An integer specifying the rolling window size (default is 30).
#' @param columns A character vector of column names to process. If NULL, numeric columns are auto-selected.
#'
#' @return A dataframe with additional columns containing outlier flags.
#'
#' @export
flag_outliers <- function(data, window_size = 30, columns = NULL) {
  if (length(data) == 0) return(data)

  result_df <- if(is.data.frame(data)) data else as.data.frame(data)

  if (is.null(columns)) {
    numeric_cols <- which(vapply(result_df, is.numeric, logical(1)))
    col_names <- names(result_df)[numeric_cols]
    exclude_pattern <- "^(ppt|precip|lat|long|elev)"
    exclude_cols <- grepl(exclude_pattern, col_names, ignore.case = TRUE)
    columns <- col_names[!exclude_cols]
    if (length(columns) == 0) {
      stop("No valid numeric columns found")
    }
  } else {
    missing_cols <- setdiff(columns, names(result_df))
    if (length(missing_cols) > 0) {
      stop("Columns not found: ", paste(missing_cols, collapse = ", "))
    }
    exclude_pattern <- "^(ppt|precip|lat|long|elev)"
    exclude_cols <- grepl(exclude_pattern, columns, ignore.case = TRUE)
    if (any(exclude_cols)) {
      warning("Excluding columns: ", paste(columns[exclude_cols], collapse = ", "))
      columns <- columns[!exclude_cols]
    }
    non_numeric <- !vapply(result_df[columns], is.numeric, logical(1))
    if (any(non_numeric)) {
      stop("Non-numeric columns: ", paste(columns[non_numeric], collapse = ", "))
    }
  }

  n_rows <- nrow(result_df)
  for(col in columns) {
    result_df[[paste0(col, "_zFlag")]] <- numeric(n_rows)
  }

  for(col in columns) {
    result_df[[paste0(col, "_zFlag")]] <- flag_outlier(result_df[[col]], window_size)
  }

  result_df
}

#' Consolidate Outlier Flags
#'
#' Combines all zFlag columns into a single `Flag` column for easier interpretation.
#'
#' @param df A dataframe containing `_zFlag` columns.
#'
#' @return A dataframe with a consolidated `Flag` column.
#'
#' @export
consolidate_outlier_flags <- function(df) {
  result_df <- data.frame(df)
  if (!is.data.frame(result_df)) {
    stop("Input must be a data frame")
  }
  flag_cols <- grep("_zFlag$", names(result_df), value = TRUE)
  if (length(flag_cols) == 0) {
    stop("No zFlag columns found in the data frame")
  }
  result_df$Flag <- character(nrow(result_df))
  for (col in flag_cols) {
    base_var <- sub("_zFlag$", "", col)
    flag_letter <- substr(base_var, 1, 4)
    result_df$Flag <- ifelse(
      result_df[[col]] == 1,
      ifelse(
        result_df$Flag == "",
        flag_letter,
        paste(result_df$Flag, flag_letter)
      ),
      result_df$Flag
    )
  }
  result_df$Flag[result_df$Flag == ""] <- NA
  result_df <- result_df[, !grepl("_zFlag$", names(result_df))]
  return(result_df)
}

#' Remove Flagged Values
#'
#' Sets flagged values in the dataframe to NA based on the `Flag` column.
#'
#' @param df A dataframe containing a `Flag` column.
#'
#' @return A dataframe with flagged values replaced by NA.
#'
#' @export
remove_outliers <- function(df) {
  if (!is.data.frame(df) || !"Flag" %in% names(df)) {
    stop("Input must be a data frame with a 'Flag' column")
  }
  result_df <- data.frame(df)
  numeric_cols <- names(result_df)[sapply(result_df, is.numeric)]
  col_mapping <- list()
  for (col in numeric_cols) {
    prefix <- tolower(substr(col, 1, 4))
    col_mapping[[prefix]] <- col
  }
  for (i in 1:nrow(result_df)) {
    if (!is.na(result_df$Flag[i])) {
      flags <- strsplit(result_df$Flag[i], " ")[[1]]
      for (flag in flags) {
        flag_prefix <- tolower(substr(flag, 1, 4))
        if (flag_prefix %in% names(col_mapping)) {
          result_df[i, col_mapping[[flag_prefix]]] <- NA
        }
      }
    }
  }
  return(result_df)
}
