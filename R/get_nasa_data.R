#' Download NASA POWER hourly data
#'
#' Downloads hourly meteorological data from the NASA POWER project for the
#' date range covered by a time-series data frame and interpolates the result
#' to the input timestamps.
#'
#' @param data A data frame or tibble containing time-series data.
#' @param datetime_col Character string specifying the date-time column in
#'   `data`. Defaults to `"dateTime"`.
#' @param site_col Optional character string specifying the site column in
#'   `data`. If `NULL`, `data` is treated as a single site.
#' @param latitude,longitude Single numeric values used for single-site data.
#'   If omitted, `data` may instead contain single-valued `latitude` and
#'   `longitude` columns. When `data` contains multiple sites, coordinates must
#'   be supplied as per-site columns in `data`.
#' @param elev_m Single numeric site elevation in meters. If omitted, `data`
#'   may instead contain a single-valued `elev_m` column. When `data` contains
#'   multiple sites, elevation must be supplied as a per-site column in `data`.
#' @param params Case-insensitive character vector of solar, meteorological, or
#'   climatology parameters to download. Defaults to `"PSC"`,
#'   `"ALLSKY_SFC_SW_DWN"`, `"PRECTOTCORR"`, and `"T2M"`. See
#'   [get_power][nasapower::get_power] for more information.
#' @param max_attempts Number of retry attempts for failed API calls. Default
#'   is 5.
#' @param quiet Logical. If `TRUE` (default), suppresses progress messages.
#' @param lat,lon `r lifecycle::badge("deprecated")` Use `latitude` and
#'   `longitude` instead. Data columns named `lat` and `lon` are also
#'   deprecated; use `latitude` and `longitude` columns instead.
#'
#' @return A tibble interpolated to the non-missing timestamps in `data`, with
#'   columns:
#'   \describe{
#'     \item{Site column}{Site names in the column named by `site_col`, when
#'       supplied}
#'     \item{dateTime}{timestamps from `data` in UTC}
#'     \item{PSC}{Elevation-corrected barometric pressure (kPa)}
#'     \item{ALLSKY_SFC_SW_DWN}{All Sky Surface Shortwave Downward Irradiance
#'       (W/m²), when requested}
#'     \item{light.obs}{Observed photosynthetically active radiation
#'       (µmol/m²/s), converted from `ALLSKY_SFC_SW_DWN` when requested}
#'     \item{T2M}{Average air temperature at 2 m above the surface (°C)}
#'     \item{PRECTOTCORR}{MERRA-2 bias corrected total precipitation (mm/hr)}
#'   }
#'
#' @examples
#' \dontrun{
#' stream_data <- data.frame(
#'   dateTime = as.POSIXct("2024-06-01 12:00:00", tz = "UTC")
#' )
#'
#' get_nasa_data(
#'   stream_data,
#'   latitude = 39.1,
#'   longitude = -96.6,
#'   elev_m = 320
#' )
#'
#' site_data <- data.frame(
#'   site = c("a", "b"),
#'   dateTime = as.POSIXct(c(
#'     "2024-06-01 12:00:00",
#'     "2024-06-01 12:00:00"
#'   ), tz = "UTC"),
#'   latitude = c(39.1, 40.0),
#'   longitude = c(-96.6, -97.2),
#'   elev_m = c(320, 340)
#' )
#'
#' get_nasa_data(site_data, site_col = "site")
#' }
#'
#' @importFrom rlang .data
#' @export
get_nasa_data <- function(
  data,
  datetime_col = "dateTime",
  site_col = NULL,
  latitude = NULL,
  longitude = NULL,
  elev_m = NULL,
  params = c("PSC", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR", "T2M"),
  max_attempts = 5,
  quiet = TRUE,
  lat = lifecycle::deprecated(),
  lon = lifecycle::deprecated()
) {
  if (lifecycle::is_present(lat)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "get_nasa_data(lat)",
      "get_nasa_data(latitude)"
    )
    if (!is.null(latitude)) {
      cli::cli_abort(
        "Use only one of {.arg latitude} and deprecated {.arg lat}."
      )
    }
    latitude <- lat
  }
  if (lifecycle::is_present(lon)) {
    lifecycle::deprecate_warn(
      "0.1.0",
      "get_nasa_data(lon)",
      "get_nasa_data(longitude)"
    )
    if (!is.null(longitude)) {
      cli::cli_abort(
        "Use only one of {.arg longitude} and deprecated {.arg lon}."
      )
    }
    longitude <- lon
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or tibble.")
  }
  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} must contain at least one row.")
  }
  if (
    !is.character(datetime_col) ||
      length(datetime_col) != 1 ||
      is.na(datetime_col) ||
      datetime_col == ""
  ) {
    cli::cli_abort("{.arg datetime_col} must be a single string.")
  }
  if (!datetime_col %in% names(data)) {
    cli::cli_abort("{.arg data} must contain a {.field {datetime_col}} column.")
  }
  if (
    !is.null(site_col) &&
      (!is.character(site_col) ||
        length(site_col) != 1 ||
        is.na(site_col) ||
        site_col == "")
  ) {
    cli::cli_abort("{.arg site_col} must be `NULL` or a single string.")
  }
  if (!is.null(site_col) && !site_col %in% names(data)) {
    cli::cli_abort("{.arg data} must contain a {.field {site_col}} column.")
  }
  if (!is.character(params) || length(params) == 0 || any(is.na(params))) {
    cli::cli_abort("{.arg params} must be a character vector.")
  }
  if (
    !is.numeric(max_attempts) ||
      length(max_attempts) != 1 ||
      is.na(max_attempts) ||
      max_attempts < 1
  ) {
    cli::cli_abort("{.arg max_attempts} must be a single positive number.")
  }
  check_bool(quiet)

  datetimes <- tryCatch(
    lubridate::as_datetime(data[[datetime_col]], tz = "UTC"),
    error = function(e) {
      cli::cli_abort(
        "{.field {datetime_col}} must contain valid dates or date-times.",
        parent = e
      )
    }
  )
  dates <- lubridate::as_date(datetimes)
  if (all(is.na(dates))) {
    cli::cli_abort(
      "{.field {datetime_col}} must contain at least one non-missing date."
    )
  }

  get_single_number <- function(x, arg, call = rlang::caller_env()) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
      cli::cli_abort(
        "{.arg {arg}} must be a single finite number.",
        call = call
      )
    }
    x
  }

  get_single_column_number <- function(
    x,
    column,
    site_label = NULL,
    call = rlang::caller_env()
  ) {
    if (!is.numeric(x)) {
      cli::cli_abort("{.field {column}} must be numeric.", call = call)
    }

    values <- unique(x[!is.na(x)])
    if (length(values) != 1 || !is.finite(values)) {
      if (is.null(site_label)) {
        cli::cli_abort(
          "{.field {column}} must contain exactly one finite value.",
          call = call
        )
      }

      cli::cli_abort(
        "{.field {column}} must contain exactly one finite value for site {.val {site_label}}.",
        call = call
      )
    }

    values
  }

  find_coordinate_column <- function(
    column,
    deprecated_column = NULL,
    call = rlang::caller_env()
  ) {
    if (column %in% names(data)) {
      return(column)
    }
    if (!is.null(deprecated_column) && deprecated_column %in% names(data)) {
      cli::cli_warn(
        c(
          "The {.field {deprecated_column}} column in {.arg data} is deprecated as of preMetabolizer 0.1.0.",
          "i" = "Use {.field {column}} instead."
        ),
        call = call
      )
      return(deprecated_column)
    }

    NULL
  }

  get_single_site_coordinate <- function(
    value,
    column,
    deprecated_column = NULL,
    call = rlang::caller_env()
  ) {
    if (!is.null(value)) {
      return(get_single_number(value, column, call = call))
    }
    actual_column <- find_coordinate_column(
      column,
      deprecated_column,
      call = call
    )
    if (!is.null(actual_column)) {
      return(get_single_column_number(
        data[[actual_column]],
        column,
        call = call
      ))
    }

    cli::cli_abort(
      "For single-site data, provide {.arg {column}} or include a single-valued {.field {column}} column in {.arg data}.",
      call = call
    )
  }

  date_range <- function(
    site_dates,
    site_label = NULL,
    call = rlang::caller_env()
  ) {
    site_dates <- site_dates[!is.na(site_dates)]
    if (length(site_dates) == 0) {
      cli::cli_abort(
        "{.field {datetime_col}} must contain at least one non-missing date for site {.val {site_label}}.",
        call = call
      )
    }

    list(
      start_date = min(site_dates),
      end_date = max(site_dates)
    )
  }

  if (is.null(site_col)) {
    site_range <- date_range(dates, "single site")
    site_requests <- list(list(
      name = NULL,
      label = "single site",
      latitude = get_single_site_coordinate(latitude, "latitude", "lat"),
      longitude = get_single_site_coordinate(longitude, "longitude", "lon"),
      elev_m = get_single_site_coordinate(elev_m, "elev_m"),
      start_date = site_range$start_date,
      end_date = site_range$end_date
    ))
  } else {
    if (any(is.na(data[[site_col]]))) {
      cli::cli_abort("{.field {site_col}} must not contain missing values.")
    }

    sites <- unique(data[[site_col]])
    single_site <- length(sites) == 1
    latitude_col <- if (single_site && !is.null(latitude)) {
      NULL
    } else {
      find_coordinate_column("latitude", "lat")
    }
    longitude_col <- if (single_site && !is.null(longitude)) {
      NULL
    } else {
      find_coordinate_column("longitude", "lon")
    }
    missing_cols <- c(
      if (is.null(latitude_col)) "latitude",
      if (is.null(longitude_col)) "longitude",
      if (!"elev_m" %in% names(data)) "elev_m"
    )
    if (!single_site && length(missing_cols) > 0) {
      cli::cli_abort(c(
        "{.arg data} must contain {.field latitude}, {.field longitude}, and {.field elev_m} columns when {.arg site_col} contains multiple sites.",
        "x" = "Missing column{?s}: {.field {missing_cols}}."
      ))
    }

    get_site_coordinate <- function(
      value,
      column,
      actual_column,
      site_rows,
      site_label,
      call = rlang::caller_env()
    ) {
      if (single_site && !is.null(value)) {
        return(get_single_number(value, column, call = call))
      }
      if (!is.null(actual_column)) {
        return(get_single_column_number(
          data[[actual_column]][site_rows],
          column,
          site_label,
          call = call
        ))
      }
      if (single_site) {
        cli::cli_abort(
          "For single-site data, provide {.arg {column}} or include a single-valued {.field {column}} column in {.arg data}.",
          call = call
        )
      }

      cli::cli_abort(
        c(
          "{.arg data} must contain {.field latitude}, {.field longitude}, and {.field elev_m} columns when {.arg site_col} contains multiple sites.",
          "x" = "Missing column: {.field {column}}."
        ),
        call = call
      )
    }

    site_requests <- vector("list", length(sites))
    for (i in seq_along(sites)) {
      site_name <- sites[[i]]
      site_label <- as.character(site_name)
      site_rows <- data[[site_col]] == site_name
      site_range <- date_range(dates[site_rows], site_label)

      site_requests[[i]] <- list(
        name = site_name,
        label = site_label,
        latitude = get_site_coordinate(
          latitude,
          "latitude",
          latitude_col,
          site_rows,
          site_label
        ),
        longitude = get_site_coordinate(
          longitude,
          "longitude",
          longitude_col,
          site_rows,
          site_label
        ),
        elev_m = get_site_coordinate(
          elev_m,
          "elev_m",
          "elev_m",
          site_rows,
          site_label
        ),
        start_date = site_range$start_date,
        end_date = site_range$end_date
      )
    }
  }

  retry_get_power <- function(..., max_attempts = 5, quiet = TRUE) {
    attempt <- 1
    while (attempt <= max_attempts) {
      result <- tryCatch(
        {
          return(get_power(...))
        },
        error = function(e) {
          if (attempt < max_attempts) {
            wait <- stats::runif(1, min = 1, max = 2) * 2^(attempt - 1)
            cli_inform_if(
              !quiet,
              c(
                "!" = "NASA POWER request attempt {attempt} failed: {e$message}",
                "i" = "Retrying in {round(wait, 2)} seconds."
              )
            )
            Sys.sleep(wait)
            NULL
          } else {
            cli::cli_abort(
              c(
                "NASA POWER request failed after {max_attempts} attempt{?s}.",
                "i" = e$message
              ),
              parent = e
            )
          }
        }
      )
      if (!is.null(result)) {
        break
      }
      attempt <- attempt + 1
    }
    result
  }

  interpolate_numeric_column <- function(
    values,
    source_datetimes,
    target_datetimes
  ) {
    output <- rep(NA_real_, length(target_datetimes))
    keep <- !is.na(values) & !is.na(source_datetimes)
    if (!any(keep)) {
      return(output)
    }

    x <- as.numeric(source_datetimes[keep])
    y <- values[keep]
    xout <- as.numeric(target_datetimes)
    unique_x <- unique(x)

    if (length(unique_x) == 1) {
      output[xout == unique_x] <- mean(y)
      return(output)
    }

    stats::approx(
      x = x,
      y = y,
      xout = xout,
      ties = mean
    )$y
  }

  interpolate_site_data <- function(site_data, target_datetimes) {
    site_data <- site_data[order(site_data$dateTime), , drop = FALSE]
    source_datetimes <- site_data$dateTime
    value_cols <- setdiff(names(site_data), c("dateTime", site_col))
    output <- tibble::tibble(dateTime = target_datetimes)

    for (column in value_cols) {
      values <- site_data[[column]]
      if (is.numeric(values)) {
        output[[column]] <- interpolate_numeric_column(
          values,
          source_datetimes,
          target_datetimes
        )
      } else {
        output[[column]] <- values[match(target_datetimes, source_datetimes)]
      }
    }

    output
  }

  interpolate_nasa_data <- function(nasa_data) {
    if (is.null(site_col)) {
      target_datetimes <- sort(unique(datetimes[!is.na(datetimes)]))
      return(interpolate_site_data(nasa_data, target_datetimes))
    }

    interpolated <- vector("list", length(sites))
    for (i in seq_along(sites)) {
      site_name <- sites[[i]]
      site_rows <- data[[site_col]] == site_name
      nasa_rows <- nasa_data[[site_col]] == site_name
      target_datetimes <- sort(unique(datetimes[site_rows & !is.na(datetimes)]))

      site_data <- interpolate_site_data(
        nasa_data[nasa_rows, , drop = FALSE],
        target_datetimes
      )
      site_data[[site_col]] <- site_name
      site_data <- site_data[c(site_col, setdiff(names(site_data), site_col))]
      interpolated[[i]] <- site_data
    }

    dplyr::bind_rows(interpolated)
  }

  cli_inform_if(!quiet, "Downloading NASA POWER data")

  all_data <- list()
  request_count <- 1

  for (site in site_requests) {
    start_date <- max(site$start_date, lubridate::ymd("2001-01-01"))
    end_date <- site$end_date

    if (start_date > end_date) {
      cli::cli_abort(
        "NASA POWER hourly data are only available from 2001-01-01 onward."
      )
    }

    years <- seq(lubridate::year(start_date), lubridate::year(end_date))

    cli_inform_if(!quiet, "Site: {.strong {site$label}}")

    for (yr in years) {
      start <- max(start_date, lubridate::ymd(paste0(yr, "-01-01")))
      end <- min(end_date, lubridate::ymd(paste0(yr, "-12-31")))

      cli_inform_if(!quiet, "Retrieving data: {start} to {end}")

      dat <- retry_get_power(
        community = "sb",
        pars = params,
        temporal_api = "hourly",
        lonlat = c(site$longitude, site$latitude),
        dates = c(start, end),
        site_elev = site$elev_m,
        time_standard = "UTC",
        max_attempts = max_attempts,
        quiet = quiet
      )

      if (!is.null(site_col)) {
        dat[[site_col]] <- site$name
      }

      all_data[[request_count]] <- dat
      request_count <- request_count + 1

      Sys.sleep(1)
    }

    cli_inform_if(
      !quiet,
      "Finished downloading data for {.strong {site$label}}"
    )
  }

  nasa_data <- dplyr::bind_rows(all_data) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      HR = paste0(
        stringr::str_pad(.data$HR, 2, side = "left", pad = "0"),
        ":00:00"
      ),
      dateTime = lubridate::ymd_hms(
        paste(.data$YYYYMMDD, .data$HR, " "),
        tz = "UTC"
      )
    ) |>
    dplyr::select(-dplyr::any_of(c("HR", "YYYYMMDD", "LON", "LAT", "YEAR")))

  if ("ALLSKY_SFC_SW_DWN" %in% names(nasa_data)) {
    nasa_data <- nasa_data |>
      dplyr::mutate(
        light.obs = streamMetabolizer::convert_SW_to_PAR(
          .data$ALLSKY_SFC_SW_DWN
        )
      )
  }

  interpolate_nasa_data(nasa_data)
}

get_power <- function(...) {
  nasapower::get_power(...)
}
