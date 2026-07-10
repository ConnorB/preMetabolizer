ncei_data_url <- "https://www.ncei.noaa.gov/access/services/data/v1"
ncei_search_url <- "https://www.ncei.noaa.gov/access/services/search/v1/data"
ncei_support_url <- "https://www.ncei.noaa.gov/access/services/support/v3/datasets"

ncei_request <- function(url) {
  httr2::request(url) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)
}

ncei_check_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (inherits(x, "Date") && length(x) == 1 && !is.na(x)) {
    return(format(x, "%Y-%m-%d"))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single date string or {.cls Date} object.",
      call = call
    )
  }
  parsed <- suppressWarnings(tryCatch(as.Date(x), error = function(e) NA))
  if (is.na(parsed)) {
    cli::cli_abort(
      "{.arg {arg}} must be a valid date in {.code YYYY-MM-DD} format.",
      call = call
    )
  }
  format(parsed, "%Y-%m-%d")
}

ncei_check_whole_number <- function(
  x,
  min = 0,
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
      "{.arg {arg}} must be a whole number between {min} and {max}.",
      call = call
    )
  }
  invisible(as.integer(x))
}

ncei_parse_stations <- function(body) {
  results <- body[["results"]]
  if (is.null(results)) {
    cli::cli_abort(
      c(
        "Unexpected response from the NCEI Search API.",
        "i" = "Expected a JSON object with a {.field results} field."
      )
    )
  }

  if (length(results) == 0) {
    return(tibble::tibble(
      station_id = character(),
      station_name = character(),
      latitude = numeric(),
      longitude = numeric(),
      start_date = as.Date(character()),
      end_date = as.Date(character())
    ))
  }

  get_station_chr <- function(res, field) {
    sta <- res[["stations"]]
    if (is.null(sta) || length(sta) == 0) {
      return(NA_character_)
    }
    val <- sta[[1]][[field]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  get_centroid <- function(res, idx) {
    centroid <- res[["centroid"]]
    # The API originally returned `centroid: {point: [lon, lat]}` but now
    # returns a bare `centroid: [lon, lat]`; accept both.
    pt <- if (is.null(centroid$point)) centroid else centroid$point
    if (is.null(pt) || length(pt) < 2) NA_real_ else as.numeric(pt[[idx]])
  }

  parse_date <- function(res, field) {
    val <- res[[field]]
    if (is.null(val)) as.Date(NA_character_) else as.Date(substr(val, 1, 10))
  }

  tibble::tibble(
    station_id = vapply(results, get_station_chr, character(1), field = "id"),
    station_name = vapply(
      results,
      get_station_chr,
      character(1),
      field = "name"
    ),
    latitude = vapply(results, get_centroid, numeric(1), idx = 2),
    longitude = vapply(results, get_centroid, numeric(1), idx = 1),
    start_date = do.call(
      c,
      lapply(results, parse_date, field = "startDate")
    ),
    end_date = do.call(
      c,
      lapply(results, parse_date, field = "endDate")
    )
  )
}

ncei_stations_total <- function(body) {
  total <- body[["totalCount"]]
  if (is.null(total)) NA_integer_ else as.integer(total)
}

ncei_parse_data_csv <- function(body, dataset) {
  col_types <- if (identical(dataset, "global-hourly")) {
    readr::cols(.default = "c")
  } else {
    NULL
  }
  data <- readr::read_csv(
    I(body),
    col_types = col_types,
    show_col_types = FALSE,
    progress = FALSE
  )

  if (nrow(data) == 0) {
    return(tibble::as_tibble(data))
  }

  if (identical(dataset, "global-hourly")) {
    data <- ncei_expand_isd_fields(data)
  }

  data <- mesonet_rename_columns(
    data,
    c(STATION = "station_id", NAME = "station_name")
  )

  if ("date" %in% names(data)) {
    d <- data[["date"]]
    if (inherits(d, "Date")) {
      # already parsed
    } else if (inherits(d, "POSIXt")) {
      data[["datetime"]] <- as.POSIXct(d, tz = "UTC")
      data[["date"]] <- NULL
    } else if (is.character(d)) {
      has_time <- any(grepl("[Tt:]", d), na.rm = TRUE)
      if (has_time) {
        data[["datetime"]] <- mesonet_parse_utc_datetime(d)
        data[["date"]] <- NULL
      } else {
        data[["date"]] <- suppressWarnings(as.Date(d))
      }
    }
  }

  passthrough <- c(
    "station_id",
    "station_name",
    "latitude",
    "longitude",
    "elevation",
    "datetime",
    "date"
  )
  lead_cols <- intersect(passthrough, names(data))
  data <- dplyr::relocate(data, dplyr::all_of(lead_cols))

  all_na <- vapply(data, function(x) all(is.na(x)), logical(1))
  data <- data[, !all_na, drop = FALSE]

  tibble::as_tibble(data)
}

ncei_isd_specs <- function() {
  list(
    WND = list(
      names = c(
        "wind_direction",
        "wind_direction_quality",
        "wind_type_code",
        "wind_speed",
        "wind_speed_quality"
      ),
      types = c("numeric", "character", "character", "numeric", "character"),
      scales = c(1, NA, NA, 10, NA),
      missing = list("999", NULL, NULL, "9999", NULL)
    ),
    CIG = list(
      names = c(
        "ceiling_height",
        "ceiling_quality",
        "ceiling_determination_code",
        "ceiling_cavok"
      ),
      types = c("numeric", "character", "character", "character"),
      scales = c(1, NA, NA, NA),
      missing = list("99999", NULL, NULL, NULL)
    ),
    VIS = list(
      names = c(
        "visibility",
        "visibility_quality",
        "visibility_variability_code",
        "visibility_variability_quality"
      ),
      types = c("numeric", "character", "character", "character"),
      scales = c(1, NA, NA, NA),
      missing = list("999999", NULL, NULL, NULL)
    ),
    TMP = list(
      names = c("temperature", "temperature_quality"),
      types = c("numeric", "character"),
      scales = c(10, NA),
      missing = list("+9999", NULL)
    ),
    DEW = list(
      names = c("dew_point_temperature", "dew_point_quality"),
      types = c("numeric", "character"),
      scales = c(10, NA),
      missing = list("+9999", NULL)
    ),
    SLP = list(
      names = c("sea_level_pressure", "sea_level_pressure_quality"),
      types = c("numeric", "character"),
      scales = c(10, NA),
      missing = list("99999", NULL)
    ),
    AA1 = list(
      names = c(
        "precipitation_period_hours",
        "precipitation",
        "precipitation_condition_code",
        "precipitation_quality"
      ),
      types = c("numeric", "numeric", "character", "character"),
      scales = c(1, 10, NA, NA),
      missing = list("99", "9999", NULL, NULL)
    )
  )
}

ncei_expand_isd_fields <- function(df) {
  specs <- ncei_isd_specs()
  aa_extras <- c("AA2", "AA3", "AA4")
  for (col in aa_extras) {
    if (col %in% names(df)) {
      spec <- specs$AA1
      suffix <- sub("^AA", "", col)
      spec$names <- paste0(spec$names, "_", suffix)
      specs[[col]] <- spec
    }
  }

  for (col in intersect(names(specs), names(df))) {
    spec <- specs[[col]]
    parsed <- ncei_split_isd(df[[col]], spec)
    df[[col]] <- NULL
    for (nm in names(parsed)) {
      df[[nm]] <- parsed[[nm]]
    }
  }

  df
}

ncei_split_isd <- function(x, spec) {
  n_fields <- length(spec$names)
  parts <- strsplit(ifelse(is.na(x), "", x), ",", fixed = TRUE)
  mat <- matrix(NA_character_, nrow = length(x), ncol = n_fields)
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (length(p) == 0 || (length(p) == 1 && !nzchar(p))) {
      next
    }
    take <- min(length(p), n_fields)
    mat[i, seq_len(take)] <- p[seq_len(take)]
  }

  out <- vector("list", n_fields)
  names(out) <- spec$names
  for (j in seq_len(n_fields)) {
    col <- mat[, j]
    miss <- spec$missing[[j]]
    if (!is.null(miss)) {
      col[col == miss] <- NA_character_
    }
    if (identical(spec$types[j], "numeric")) {
      val <- suppressWarnings(as.numeric(col))
      scale <- spec$scales[j]
      if (!is.na(scale) && scale != 1) {
        val <- val / scale
      }
      out[[j]] <- val
    } else {
      out[[j]] <- col
    }
  }
  out
}
