check_bool <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be `TRUE` or `FALSE`.", call = call)
  }

  invisible(x)
}

check_string <- function(
  x,
  allow_null = FALSE,
  allow_empty = TRUE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single string.", call = call)
  }

  if (!allow_empty && !nzchar(x)) {
    cli::cli_abort("{.arg {arg}} must be a non-empty string.", call = call)
  }

  invisible(x)
}

# Validate a site coordinate (latitude/longitude) supplied alongside a vector
# of `n` timestamps. Allows a single value (one site, recycled across all
# timestamps) or a length-`n` value (one site per timestamp). Rejects any other
# length so a silent recycling mistake becomes a clear error.
check_site_coord <- function(
  x,
  n,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  check_numeric(x, allow_na = FALSE, arg = arg, call = call)

  if (length(x) != 1L && length(x) != n) {
    cli::cli_abort(
      "{.arg {arg}} must be length 1 or {n}, not length {length(x)}.",
      call = call
    )
  }

  invisible(x)
}

check_character <- function(
  x,
  allow_null = FALSE,
  allow_empty = TRUE,
  allow_na = TRUE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || length(x) == 0) {
    cli::cli_abort("{.arg {arg}} must be a character vector.", call = call)
  }

  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain missing values.", call = call)
  }

  if (!allow_empty && any(!nzchar(x))) {
    cli::cli_abort("{.arg {arg}} must not contain empty strings.", call = call)
  }

  invisible(x)
}

check_numeric <- function(
  x,
  allow_null = FALSE,
  allow_na = TRUE,
  allow_infinite = FALSE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be numeric.", call = call)
  }

  if (!allow_na && anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must not contain missing values.", call = call)
  }

  if (!allow_infinite && any(is.infinite(x))) {
    cli::cli_abort("{.arg {arg}} must contain only finite values.", call = call)
  }

  invisible(x)
}

# Resolve the name of a date-time column in `data`. When `datetime_col` is
# supplied it is validated against `names(data)`; otherwise the single column
# inheriting one of `classes` is detected. Class checking of an explicitly
# named column is left to the caller.
detect_datetime_col <- function(
  data,
  datetime_col = NULL,
  classes = "POSIXct",
  data_arg = "data",
  arg = "datetime_col",
  call = rlang::caller_env()
) {
  if (!is.null(datetime_col)) {
    check_string(datetime_col, allow_empty = FALSE, arg = arg, call = call)
    if (!datetime_col %in% names(data)) {
      cli::cli_abort(
        "{.arg {data_arg}} must contain a {.field {datetime_col}} column.",
        call = call
      )
    }
    return(datetime_col)
  }

  is_datetime <- vapply(data, \(x) inherits(x, classes), logical(1))
  candidates <- names(data)[is_datetime]

  if (length(candidates) == 0) {
    cli::cli_abort(
      c(
        "{.arg {data_arg}} must contain a date-time column ({.cls {classes}}).",
        "i" = "Alternatively, name the column with {.arg {arg}}."
      ),
      call = call
    )
  }
  if (length(candidates) > 1) {
    cli::cli_abort(
      c(
        "{.arg {data_arg}} contains multiple date-time columns: {.field {candidates}}.",
        "i" = "Specify the column to use with {.arg {arg}}."
      ),
      call = call
    )
  }

  candidates
}

cli_inform_if <- function(condition, message, .envir = parent.frame()) {
  if (isTRUE(condition)) {
    cli::cli_inform(message, .envir = .envir)
  }

  invisible(NULL)
}
