iem_base_url <- "https://mesonet.agron.iastate.edu/api/1"

iem_request <- function(endpoint) {
  httr2::request(iem_base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3)
}

iem_perform_json <- function(request, error_message) {
  tryCatch(
    {
      request |>
        http_req_perform() |>
        httr2::resp_body_json(simplifyVector = TRUE)
    },
    error = function(e) {
      cli::cli_abort(error_message, parent = e)
    }
  )
}

iem_as_tibble <- function(response) {
  if (is.null(response$data)) {
    data <- tibble::tibble()
  } else {
    data <- tibble::as_tibble(response$data)
  }

  attr(data, "schema") <- response$schema
  data
}

iem_parse_datetime <- function(x) {
  mesonet_parse_utc_datetime(x)
}

iem_parse_time_columns <- function(data) {
  datetime_cols <- intersect(
    names(data),
    c(
      "archive_begin",
      "archive_end",
      "modified",
      "utc_valid",
      "windrose_update"
    )
  )

  for (col in datetime_cols) {
    data[[col]] <- iem_parse_datetime(data[[col]])
  }

  date_cols <- intersect(names(data), c("day", "date", "local_date"))
  for (col in date_cols) {
    data[[col]] <- as.Date(data[[col]])
  }

  data
}

iem_check_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (inherits(x, "Date") && length(x) == 1 && !is.na(x)) {
    return(format(x))
  }

  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single date string or `Date` object.",
      call = call
    )
  }

  parsed <- suppressWarnings(tryCatch(as.Date(x), error = function(e) NA))
  if (is.na(parsed)) {
    cli::cli_abort(
      "{.arg {arg}} must be a valid date in `YYYY-MM-DD` format.",
      call = call
    )
  }

  format(parsed)
}

iem_check_whole_number <- function(
  x,
  min = 1,
  max = Inf,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (
    !is.numeric(x) ||
      length(x) != 1 ||
      is.na(x) ||
      !is.finite(x) ||
      x != as.integer(x) ||
      x < min ||
      x > max
  ) {
    if (is.infinite(max)) {
      cli::cli_abort(
        "{.arg {arg}} must be a single whole number greater than or equal to {min}.",
        call = call
      )
    }

    cli::cli_abort(
      "{.arg {arg}} must be a single whole number between {min} and {max}.",
      call = call
    )
  }

  invisible(x)
}
