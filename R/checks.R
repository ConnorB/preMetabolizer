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

cli_inform_if <- function(condition, message) {
  if (isTRUE(condition)) {
    cli::cli_inform(message)
  }

  invisible(NULL)
}
