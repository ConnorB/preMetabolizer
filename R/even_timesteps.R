#' Align logger data to even, rounded timestamps
#'
#' Snaps logger data onto an evenly spaced, rounded time grid. The time step
#' is inferred from the data, the grid is aligned to round times (e.g. a
#' series of 10-minute readings starting at 20:59 yields 21:00, 21:10, ...),
#' and values are linearly interpolated onto the grid timestamps. Only the
#' grid timestamps are returned; grid times that fall in data gaps wider than
#' `max_gap` are left `NA`.
#'
#' @param logger_data Data frame or tibble containing timestamped logger data.
#' @param datetime_col Optional character string naming the POSIXct datetime
#'   column. If `NULL` (default), the single POSIXct column in `logger_data`
#'   is detected automatically; when `logger_data` contains more than one
#'   POSIXct column, `datetime_col` must be supplied.
#' @param site_col Optional character string naming a site column. When
#'   supplied, each site is aligned independently.
#' @param max_gap Optional single number giving the maximum gap, in seconds,
#'   across which values are interpolated. A grid timestamp is filled only
#'   when the observations bracketing it are at most `max_gap` seconds apart;
#'   non-numeric columns use the nearest observation within `max_gap / 2`
#'   seconds. Defaults to 1.5 times the inferred time step, which bridges
#'   ordinary clock offsets while leaving missing readings as `NA`. Use `Inf`
#'   to interpolate across all gaps.
#' @param loggerData `r lifecycle::badge("deprecated")` Use `logger_data`
#'   instead.
#'
#' @return A data frame or tibble, matching the input class, containing only
#'   the evenly spaced, rounded timestamps with values interpolated from the
#'   original observations. Rows with missing timestamps or missing site
#'   values are dropped with a warning before the grid is built. Sites with
#'   fewer than two distinct non-missing timestamps are dropped because an
#'   evenly spaced grid cannot be inferred.
#'
#' @details
#' The time step is inferred from the modal difference between the sorted
#' unique timestamps for each site. Differences are rounded to the nearest
#' whole second, and small clock offsets around minute-level intervals are
#' snapped to the nearest minute. The grid runs from the first timestamp
#' rounded to the nearest step through the last timestamp rounded to the nearest
#' step, so slightly early or late boundary observations are retained. Numeric
#' and POSIXct columns are linearly interpolated onto the grid; other columns
#' are filled from the nearest observation, with a warning. Use this after
#' removing obvious duplicate or invalid timestamps.
#'
#' @examples
#' # Align slightly offset 10-minute readings to round timestamps.
#' logger <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2024-01-01 20:59:33",
#'       "2024-01-01 21:09:31",
#'       "2024-01-01 21:19:29",
#'       "2024-01-01 21:29:37"
#'     ),
#'     tz = "UTC"
#'   ),
#'   temperature = c(10.0, 12.0, 14.0, 16.0)
#' )
#'
#' even_timesteps(logger)
#'
#' # Numeric values are linearly interpolated onto the rounded grid.
#' irregular <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2024-01-01 00:01:15",
#'       "2024-01-01 00:11:06",
#'       "2024-01-01 00:21:43"
#'     ),
#'     tz = "UTC"
#'   ),
#'   temperature = c(10, 20, 30)
#' )
#'
#' even_timesteps(irregular)
#'
#' # By default, values are not interpolated across a missing reading.
#' with_gap <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2024-01-01 00:00:00",
#'       "2024-01-01 00:10:00",
#'       "2024-01-01 00:30:00",
#'       "2024-01-01 00:40:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   temperature = c(10, 12, 16, 18)
#' )
#'
#' even_timesteps(with_gap)
#'
#' # Increase max_gap to interpolate across the 20-minute gap.
#' even_timesteps(with_gap, max_gap = 20 * 60)
#'
#' # Align each site independently when sites have different sampling
#' # intervals.
#' multiple_sites <- data.frame(
#'   site = c("A", "A", "A", "B", "B", "B"),
#'   datetime = as.POSIXct(
#'     c(
#'       "2024-01-01 00:01:00",
#'       "2024-01-01 00:11:00",
#'       "2024-01-01 00:21:00",
#'       "2024-01-01 00:02:00",
#'       "2024-01-01 00:32:00",
#'       "2024-01-01 01:02:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   temperature = c(10, 11, 12, 20, 21, 22)
#' )
#'
#' even_timesteps(
#'   multiple_sites,
#'   datetime_col = "datetime",
#'   site_col = "site"
#' )
#'
#' # Supply datetime_col when the data contains multiple POSIXct columns.
#' multiple_datetimes <- data.frame(
#'   date_time_utc = as.POSIXct(
#'     c(
#'       "2024-01-01 00:01:00",
#'       "2024-01-01 00:11:00",
#'       "2024-01-01 00:21:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   date_time_central = as.POSIXct(
#'     c(
#'       "2024-01-02 08:00:00",
#'       "2024-01-02 08:01:00",
#'       "2024-01-02 08:02:00"
#'     ),
#'     tz = "US/Central"
#'   ),
#'   temperature = c(10, 11, 12)
#' )
#'
#' even_timesteps(
#'   multiple_datetimes,
#'   datetime_col = "date_time_utc"
#' )
#'
#' # Exact observations are retained.
#' exact_times <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2024-01-01 00:00:00",
#'       "2024-01-01 00:10:00",
#'       "2024-01-01 00:20:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   temperature = c(10, 99, 20)
#' )
#'
#' even_timesteps(exact_times)
#'
#' @export
even_timesteps <- function(
  logger_data,
  datetime_col = NULL,
  site_col = NULL,
  max_gap = NULL,
  loggerData = lifecycle::deprecated()
) {
  if (lifecycle::is_present(loggerData)) {
    lifecycle::deprecate_soft(
      "0.0.0.9000",
      "even_timesteps(loggerData)",
      "even_timesteps(logger_data)"
    )
    logger_data <- loggerData
  }

  # Input validation
  if (!is.data.frame(logger_data)) {
    cli::cli_abort("{.arg logger_data} must be a data frame.")
  }
  datetime_col <- detect_datetime_col(
    logger_data,
    datetime_col,
    classes = "POSIXct",
    data_arg = "logger_data"
  )
  check_string(site_col, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(site_col)) {
    if (!site_col %in% names(logger_data)) {
      cli::cli_abort(
        "{.arg logger_data} must contain a {.field {site_col}} column."
      )
    }
  }
  if (!is.null(max_gap)) {
    if (
      !is.numeric(max_gap) ||
        length(max_gap) != 1 ||
        is.na(max_gap) ||
        max_gap <= 0
    ) {
      cli::cli_abort(
        "{.arg max_gap} must be a single positive number of seconds."
      )
    }
  }

  # Get datetime vector
  datetime_vec <- logger_data[[datetime_col]]
  if (!inherits(datetime_vec, "POSIXct")) {
    cli::cli_abort("{.field {datetime_col}} must be a POSIXct vector.")
  }
  missing_time <- is.na(datetime_vec)
  if (any(missing_time)) {
    n_missing_time <- sum(missing_time)
    cli::cli_warn(
      "{.field {datetime_col}} contains {n_missing_time} missing value{?s}; dropping affected row{?s}."
    )
    logger_data <- logger_data[!missing_time, , drop = FALSE]
    datetime_vec <- logger_data[[datetime_col]]
  }
  if (nrow(logger_data) == 0) {
    cli::cli_abort(
      "{.arg logger_data} contains no rows after dropping missing timestamps."
    )
  }
  if (!is.null(site_col)) {
    missing_site <- is.na(logger_data[[site_col]])
    if (any(missing_site)) {
      n_missing_site <- sum(missing_site)
      cli::cli_warn(
        "{.field {site_col}} contains {n_missing_site} missing value{?s}; dropping affected row{?s}."
      )
      logger_data <- logger_data[!missing_site, , drop = FALSE]
      datetime_vec <- logger_data[[datetime_col]]
    }
    if (nrow(logger_data) == 0) {
      cli::cli_abort(
        "{.arg logger_data} contains no rows after dropping missing site values."
      )
    }
  }

  value_cols <- setdiff(names(logger_data), c(datetime_col, site_col))
  nearest_cols <- value_cols[
    !vapply(
      logger_data[value_cols],
      function(x) is.numeric(x) || inherits(x, "POSIXct"),
      logical(1)
    )
  ]
  if (length(nearest_cols) > 0) {
    cli::cli_warn(c(
      "Column{?s} {.field {nearest_cols}} {?is/are} not numeric and cannot be interpolated.",
      "i" = "Values are filled from the nearest observation within {.code max_gap / 2} seconds."
    ))
  }

  # Function to process a single site
  process_site <- function(site_data) {
    # Find the most common time interval. Using the modal diff (rather than
    # the first one) is robust to near-duplicate timestamps and gaps that
    # would otherwise mis-infer the step.
    timestamps <- site_data[[datetime_col]]
    interval_seconds <- infer_interval_seconds(timestamps)
    if (is.na(interval_seconds)) {
      warn_insufficient_site(site_data, site_col)
      return(site_data[0, , drop = FALSE])
    }
    gap_limit <- if (is.null(max_gap)) 1.5 * interval_seconds else max_gap

    # Create a sequence of evenly spaced timestamps aligned to the inferred
    # interval (e.g. 20:59 rounds up to 21:00 for 10-minute data).
    even_times <- build_time_grid(timestamps, interval_seconds)

    times_num <- round(as.numeric(timestamps))
    grid_num <- as.numeric(even_times)

    result <- data.frame(temp_col = even_times)
    names(result) <- datetime_col
    if (!is.null(site_col)) {
      result[[site_col]] <- site_data[[site_col]][1]
    }
    for (col in setdiff(names(site_data), c(datetime_col, site_col))) {
      values <- site_data[[col]]
      result[[col]] <- if (is.numeric(values) || inherits(values, "POSIXct")) {
        interp_linear(values, times_num, grid_num, gap_limit)
      } else {
        interp_nearest(values, times_num, grid_num, gap_limit)
      }
    }
    result
  }

  if (is.null(site_col)) {
    # Process all data as single site
    result <- process_site(logger_data)
  } else {
    # Split by site, process each separately, and recombine
    site_groups <- split(logger_data, logger_data[[site_col]], drop = TRUE)
    result <- do.call(
      rbind,
      lapply(site_groups, process_site)
    )
  }

  # Reorder columns (datetime and site first)
  col_order <- unique(c(datetime_col, site_col, names(logger_data)))
  result <- result[, col_order[col_order %in% names(result)], drop = FALSE]
  rownames(result) <- NULL

  # Return the same class as the input: tibble or data frame
  if (tibble::is_tibble(logger_data)) {
    result <- tibble::as_tibble(result)
  } else {
    result <- as.data.frame(result, stringsAsFactors = FALSE)
  }

  result
}

infer_interval_seconds <- function(timestamps) {
  timestamps <- sort(unique(timestamps[!is.na(timestamps)]))
  diffs_sec <- round(as.numeric(diff(timestamps), units = "secs"))
  diffs_sec <- normalize_interval_seconds(diffs_sec)
  diffs_sec <- diffs_sec[diffs_sec > 0]
  if (length(diffs_sec) == 0) {
    return(NA_real_)
  }

  interval_seconds <- as.numeric(calc_mode(diffs_sec, multi = "first"))
  if (is.na(interval_seconds) || interval_seconds <= 0) {
    return(NA_real_)
  }
  interval_seconds
}

normalize_interval_seconds <- function(seconds) {
  nearest_minute <- round(seconds / 60) * 60
  snap_to_minute <- seconds >= 5 * 60 &
    abs(seconds - nearest_minute) <= 30
  seconds[snap_to_minute] <- nearest_minute[snap_to_minute]
  seconds
}

build_time_grid <- function(timestamps, interval_seconds) {
  tz <- timestamp_tzone(timestamps)
  timestamps_num <- round(as.numeric(timestamps))
  min_num <- min(timestamps_num, na.rm = TRUE)
  max_num <- max(timestamps_num, na.rm = TRUE)
  if (!is.finite(min_num) || !is.finite(max_num)) {
    return(as.POSIXct(numeric(), origin = "1970-01-01", tz = tz))
  }

  if (interval_seconds %% 86400 == 0) {
    return(build_day_grid(min_num, max_num, interval_seconds, tz))
  }

  origin_num <- local_midnight_num(min_num, tz)
  start_num <- origin_num +
    nearest_to_interval(min_num - origin_num, interval_seconds)
  end_num <- origin_num +
    nearest_to_interval(max_num - origin_num, interval_seconds)
  grid_num <- seq_seconds(start_num, end_num, interval_seconds)
  as.POSIXct(grid_num, origin = "1970-01-01", tz = tz)
}

build_day_grid <- function(min_num, max_num, interval_seconds, tz) {
  step_days <- interval_seconds / 86400
  origin_day <- nearest_local_day_number(min_num, tz)
  max_day <- nearest_local_day_number(max_num, tz)
  end_day <- origin_day +
    nearest_to_interval(max_day - origin_day, step_days)
  start_day <- origin_day
  if (start_day > end_day) {
    return(as.POSIXct(numeric(), origin = "1970-01-01", tz = tz))
  }

  days <- seq_seconds(start_day, end_day, step_days)
  dates <- as.Date(days, origin = "1970-01-01")
  as.POSIXct(paste(dates, "00:00:00"), tz = tz)
}

nearest_local_day_number <- function(time_num, tz) {
  time <- as.POSIXct(time_num, origin = "1970-01-01", tz = tz)
  time_lt <- as.POSIXlt(time, tz = tz)
  day <- as.numeric(as.Date(time, tz = tz))
  seconds_into_day <- time_lt$hour * 3600 + time_lt$min * 60 + time_lt$sec
  if (seconds_into_day >= 12 * 3600) {
    day <- day + 1
  }
  day
}

local_midnight_num <- function(time_num, tz) {
  time <- as.POSIXct(time_num, origin = "1970-01-01", tz = tz)
  date <- as.Date(time, tz = tz)
  as.numeric(as.POSIXct(paste(date, "00:00:00"), tz = tz))
}

ceiling_to_interval <- function(x, interval) {
  ceiling(x / interval) * interval
}

floor_to_interval <- function(x, interval) {
  floor(x / interval) * interval
}

nearest_to_interval <- function(x, interval) {
  floor(x / interval + 0.5) * interval
}

seq_seconds <- function(start, end, by) {
  if (start > end) {
    return(numeric())
  }
  n_steps <- floor((end - start) / by)
  out <- start + seq.int(0, n_steps) * by
  if (length(out) > 0 && utils::tail(out, 1) < end) {
    quotient <- (end - start) / by
    if (abs(quotient - round(quotient)) < sqrt(.Machine$double.eps)) {
      out <- c(out, end)
    }
  }
  out
}

timestamp_tzone <- function(x) {
  tz <- attr(x, "tzone", exact = TRUE)
  if (is.null(tz) || length(tz) == 0 || is.na(tz[[1]])) {
    return("")
  }
  tz[[1]]
}

warn_insufficient_site <- function(site_data, site_col) {
  if (is.null(site_col)) {
    cli::cli_warn(c(
      "Data has fewer than two distinct non-missing timestamps; dropping {nrow(site_data)} row{?s}.",
      "i" = "An evenly spaced grid cannot be inferred."
    ))
    return(invisible(NULL))
  }

  site <- as.character(site_data[[site_col]][1])
  cli::cli_warn(c(
    "Site {.val {site}} has fewer than two distinct non-missing timestamps; dropping {nrow(site_data)} row{?s}.",
    "i" = "An evenly spaced grid cannot be inferred."
  ))
  invisible(NULL)
}

# Linearly interpolate a numeric or POSIXct column onto the grid. Grid points
# whose bracketing non-NA observations are more than `max_gap` seconds apart
# are left NA, so values are never fabricated across data gaps; grid points
# that coincide exactly with an observation keep the observed value.
interp_linear <- function(values, times_num, grid_num, max_gap) {
  out <- rep(NA_real_, length(grid_num))
  ok <- !is.na(values)
  if (any(ok)) {
    x <- times_num[ok]
    y <- as.numeric(values[ok])
    unique_x <- unique(x)
  }
  if (any(ok) && length(unique_x) >= 2) {
    out <- stats::approx(
      x,
      y,
      xout = grid_num,
      method = "linear",
      ties = mean,
      rule = 1
    )$y
    prev <- stats::approx(
      x,
      x,
      xout = grid_num,
      method = "constant",
      f = 0,
      ties = mean,
      rule = 1
    )$y
    nxt <- stats::approx(
      x,
      x,
      xout = grid_num,
      method = "constant",
      f = 1,
      ties = mean,
      rule = 1
    )$y
    gap <- nxt - prev
    out[is.na(gap) | gap > max_gap] <- NA_real_

    missing <- which(is.na(out))
    if (length(missing) > 0) {
      for (grid_idx in missing) {
        value_idx <- which.min(abs(x - grid_num[[grid_idx]]))
        if (abs(x[[value_idx]] - grid_num[[grid_idx]]) <= max_gap / 2) {
          out[[grid_idx]] <- y[[value_idx]]
        }
      }
    }
  } else if (any(ok)) {
    idx <- nearest_grid_index(unique_x, grid_num, max_gap / 2)
    if (!is.na(idx)) {
      out[idx] <- mean(y, na.rm = TRUE)
    }
  }
  if (inherits(values, "POSIXct")) {
    tzone <- attr(values, "tzone")
    if (is.null(tzone)) {
      tzone <- ""
    }
    out <- as.POSIXct(out, origin = "1970-01-01", tz = tzone)
  }
  out
}

nearest_grid_index <- function(time_num, grid_num, max_distance) {
  if (length(grid_num) == 0) {
    return(NA_integer_)
  }
  distances <- abs(grid_num - time_num)
  idx <- which.min(distances)
  if (length(idx) == 0 || distances[[idx]] > max_distance) {
    return(NA_integer_)
  }
  idx
}

# Fill a column that cannot be interpolated (character, factor, logical, ...)
# with the value of the nearest observation, provided it is within
# `max_gap / 2` seconds of the grid point.
interp_nearest <- function(values, times_num, grid_num, max_gap) {
  out <- values[rep(NA_integer_, length(grid_num))]
  ok <- !is.na(values)
  if (!any(ok)) {
    return(out)
  }
  ord <- order(times_num[ok])
  x <- times_num[ok][ord]
  y <- values[ok][ord]
  idx_prev <- findInterval(grid_num, x)
  idx_next <- idx_prev + 1L
  dist_prev <- ifelse(idx_prev >= 1L, grid_num - x[pmax(idx_prev, 1L)], Inf)
  dist_next <- ifelse(
    idx_next <= length(x),
    x[pmin(idx_next, length(x))] - grid_num,
    Inf
  )
  use_prev <- dist_prev <= dist_next
  idx <- ifelse(use_prev, idx_prev, idx_next)
  fill <- pmin(dist_prev, dist_next) <= max_gap / 2
  out[fill] <- y[idx[fill]]
  out
}
