cdo_base_url <- "https://www.ncei.noaa.gov/cdo-web/api/v2"

cdo_daily_limit <- 10000L
cdo_per_second_limit <- 5L

.cdo_state <- new.env(parent = emptyenv())
.cdo_state$count <- 0L
.cdo_state$warned <- FALSE

#' Inspect and reset the NCEI CDO session request counter
#'
#' The NCEI CDO Web Services v2 API enforces a daily limit of 10,000
#' requests per token. The package counts every successful CDO request
#' made in the current R session and aborts further requests once the
#' limit is reached. These helpers expose the counter.
#'
#' Because the API does not report remaining quota, the counter is
#' session-local and best-effort: it does not include requests made by
#' other R sessions, other tools, or earlier sessions on the same day.
#'
#' @return `cdo_request_count()` returns an integer count of successful
#'   CDO requests made this session. `cdo_reset_request_count()` is
#'   called for its side effect of zeroing the counter and returns the
#'   prior count invisibly.
#'
#' @seealso [cdo_data()] and friends.
#'
#' @export
cdo_request_count <- function() {
  .cdo_state$count
}

#' @rdname cdo_request_count
#' @export
cdo_reset_request_count <- function() {
  prior <- .cdo_state$count
  .cdo_state$count <- 0L
  .cdo_state$warned <- FALSE
  invisible(prior)
}

cdo_check_quota <- function(call = rlang::caller_env()) {
  if (.cdo_state$count >= cdo_daily_limit) {
    cli::cli_abort(
      c(
        "Reached the NCEI CDO daily request limit of {cdo_daily_limit} requests.",
        "i" = "The limit resets daily; call {.fn cdo_reset_request_count} once you are within a new quota window."
      ),
      call = call
    )
  }
  invisible(NULL)
}

cdo_record_request <- function() {
  .cdo_state$count <- .cdo_state$count + 1L
  remaining <- cdo_daily_limit - .cdo_state$count
  if (remaining < 1000L && !.cdo_state$warned) {
    .cdo_state$warned <- TRUE
    cli::cli_alert_warning(
      "Approaching the NCEI CDO daily request limit ({remaining} requests remaining this session)."
    )
  }
  invisible(.cdo_state$count)
}

cdo_token <- function(call = rlang::caller_env()) {
  token <- Sys.getenv("API_NCEI_CDO", "")
  if (!nzchar(token)) {
    cli::cli_abort(
      c(
        "NCEI CDO API token not found.",
        "i" = "Set the {.envvar API_NCEI_CDO} environment variable.",
        "i" = "Request a free token at {.url https://www.ncdc.noaa.gov/cdo-web/token}."
      ),
      call = call
    )
  }
  token
}

cdo_request <- function(path, token = cdo_token()) {
  url <- paste0(cdo_base_url, path)
  httr2::request(url) |>
    httr2::req_headers(token = token) |>
    httr2::req_user_agent(
      "preMetabolizer (https://github.com/ConnorB/preMetabolizer)"
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_throttle(
      capacity = cdo_per_second_limit,
      fill_time_s = 1,
      realm = "ncei-cdo"
    )
}

cdo_perform <- function(req, error_message) {
  cdo_check_quota()
  tryCatch(
    {
      resp <- httr2::req_perform(req)
      cdo_record_request()
      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },
    error = function(e) {
      cli::cli_abort(error_message, parent = e)
    }
  )
}

cdo_check_max_results <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (
    length(x) != 1 ||
      is.na(x) ||
      !is.numeric(x) ||
      x < 1 ||
      (is.finite(x) && x != as.integer(x))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive whole number or {.code Inf}.",
      call = call
    )
  }
  if (is.finite(x)) as.integer(x) else Inf
}

cdo_paginate <- function(
  req,
  max_results = Inf,
  page_size = 1000L,
  error_message
) {
  page_size <- as.integer(page_size)
  if (is.finite(max_results)) {
    page_size <- min(page_size, max_results)
  }

  results <- list()
  offset <- 1L
  total <- NA_integer_

  repeat {
    page_req <- req |>
      httr2::req_url_query(limit = page_size, offset = offset)
    body <- cdo_perform(page_req, error_message)

    page <- body[["results"]]
    if (is.null(page) || (is.data.frame(page) && nrow(page) == 0)) {
      break
    }

    results[[length(results) + 1L]] <- page

    n <- if (is.data.frame(page)) nrow(page) else length(page)
    offset <- offset + n

    meta_total <- tryCatch(
      as.integer(body[["metadata"]][["resultset"]][["count"]]),
      error = function(e) NA_integer_
    )
    if (length(meta_total) == 1 && !is.na(meta_total)) {
      total <- meta_total
    }

    fetched <- sum(vapply(
      results,
      function(p) {
        if (is.data.frame(p)) nrow(p) else length(p)
      },
      integer(1)
    ))

    if (fetched >= max_results) {
      break
    }
    if (!is.na(total) && fetched >= total) {
      break
    }
    if (n < page_size) break
  }

  if (length(results) == 0) {
    return(tibble::tibble())
  }

  combined <- dplyr::bind_rows(lapply(results, tibble::as_tibble))
  if (is.finite(max_results) && nrow(combined) > max_results) {
    combined <- combined[seq_len(max_results), , drop = FALSE]
  }
  cdo_cast_columns(combined)
}

cdo_cast_columns <- function(df) {
  for (col in intersect(c("mindate", "maxdate"), names(df))) {
    df[[col]] <- suppressWarnings(as.Date(df[[col]]))
  }
  if ("date" %in% names(df) && is.character(df[["date"]])) {
    has_time <- any(grepl("[Tt:]", df[["date"]]), na.rm = TRUE)
    if (has_time) {
      df[["date"]] <- mesonet_parse_utc_datetime(df[["date"]])
    } else {
      df[["date"]] <- suppressWarnings(as.Date(df[["date"]]))
    }
  }
  for (col in intersect(
    c("latitude", "longitude", "elevation", "value", "datacoverage"),
    names(df)
  )) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  tibble::as_tibble(df)
}

cdo_collapse <- function(x) {
  if (is.null(x)) NULL else paste(x, collapse = ",")
}
