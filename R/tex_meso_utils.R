tex_meso_base_url <- "https://www.texmesonet.org/api"

tex_meso_request <- function(endpoint) {
  httr2::request(tex_meso_base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3)
}

tex_meso_perform_json <- function(request, error_message) {
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

tex_meso_parse_datetime <- function(x) {
  mesonet_parse_utc_datetime(x)
}

tex_meso_parse_numeric_columns <- function(data, exclude = character()) {
  for (col in setdiff(names(data), exclude)) {
    if (!is.character(data[[col]])) {
      next
    }

    values <- trimws(data[[col]])
    empty <- is.na(values) | values == ""
    numeric_values <- suppressWarnings(as.numeric(values))

    if (all(empty | !is.na(numeric_values))) {
      numeric_values[empty] <- NA_real_
      data[[col]] <- numeric_values
    }
  }

  data
}

tex_meso_as_tibble <- function(
  data,
  numeric_exclude = character(),
  names = character()
) {
  data |>
    tibble::as_tibble() |>
    tex_meso_parse_numeric_columns(exclude = numeric_exclude) |>
    mesonet_rename_columns(names)
}

tex_meso_check_positive_int <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (
    !is.numeric(x) ||
      length(x) != 1 ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0 ||
      x != as.integer(x)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive whole number.",
      call = call
    )
  }

  invisible(x)
}
