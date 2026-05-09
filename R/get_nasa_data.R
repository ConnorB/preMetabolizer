#' Download NASA POWER Hourly Data
#'
#' Downloads hourly meteorological data from the NASA POWER project for a set of sites
#' over specified time periods.
#'
#' @param site_meta A data frame containing metadata for each site. Required columns:
#'   \itemize{
#'     \item `Site`: site name (character)
#'     \item `lat`: latitude (numeric)
#'     \item `lon`: longitude (numeric)
#'     \item `elev_m`: elevation in meters (numeric)
#'     \item `start_date`, `end_date`: date range (can be `Date` or character)
#'   }
#'
#' @param params Case-insensitive character vector of solar, meteorological or climatology parameters to download. Defaults to "PSC", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR", and "T2M". See [get_power][nasapower::get_power] for more information.
#' @param max_attempts Number of retry attempts for failed API calls. Default is 5.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{Site}{Site name}
#'     \item{dateTime}{timestamp in UTC}
#'     \item{PSC}{Elevation-corrected barometric pressure (kPa)}
#'     \item{ALLSKY_SFC_SW_DWN}{All Sky Surface Shortwave Downward Irradiance (W/m²)}
#'     \item{T2M}{Average air temperature at 2 m above the surface (°C)}
#'     \item{PRECTOTCORR}{MERRA-2 bias corrected total precipitation(mm/hr)}
#'   }
#'
#' @importFrom nasapower get_power
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom lubridate as_date ymd year ymd_hms
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble as_tibble
#' @importFrom stringr str_pad
#' @importFrom stats runif
#' @export
get_nasa_data <- function(
  site_meta,
  params = c("PSC", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR", "T2M"),
  max_attempts = 5
) {
  cli::cli_h1("Downloading NASA POWER data")

  retry_get_power <- function(..., max_attempts = 5) {
    attempt <- 1
    while (attempt <= max_attempts) {
      result <- tryCatch(
        {
          return(get_power(...))
        },
        error = function(e) {
          if (attempt < max_attempts) {
            wait <- runif(1, min = 1, max = 2) * 2^(attempt - 1)
            cli::cli_alert_warning("    Attempt {attempt} failed: {e$message}")
            cli::cli_alert_info("    Retrying in {round(wait, 2)} seconds...")
            Sys.sleep(wait)
            NULL
          } else {
            cli::cli_alert_danger(
              "    Failed after {max_attempts} attempts: {e$message}"
            )
            stop(e)
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

  all_data <- list()

  for (i in seq_len(nrow(site_meta))) {
    site <- site_meta[i, ]

    site_name <- site$Site
    lat <- site$lat
    lon <- site$lon
    elev_m <- site$elev_m
    raw_sDate <- lubridate::as_date(site$start_date)
    eDate <- lubridate::as_date(site$end_date)

    # Ensure start date is not before 2001-01-01
    sDate <- max(raw_sDate, lubridate::ymd("2001-01-01"))

    years <- seq(lubridate::year(sDate), lubridate::year(eDate))

    cli::cli_h2("Site: {.strong {site_name}}")

    for (yr in years) {
      start <- max(sDate, lubridate::ymd(paste0(yr, "-01-01")))
      end <- min(eDate, lubridate::ymd(paste0(yr, "-12-31")))

      cli::cli_alert_info("  Retrieving data: {start} to {end}")

      dat <- retry_get_power(
        community = "sb",
        pars = params,
        temporal_api = "hourly",
        lonlat = c(lon, lat),
        dates = c(start, end),
        site_elev = elev_m,
        time_standard = "UTC",
        max_attempts = max_attempts
      )

      dat$Site <- site_name
      all_data[[paste0(site_name, "_", yr)]] <- dat

      Sys.sleep(1)
    }

    cli::cli_alert_success(
      "Finished downloading data for {.strong {site_name}}"
    )
  }

  all_data |>
    dplyr::bind_rows() |>
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
  # dplyr::select(
  #   .data$Site, .data$dateTime, .data$PSC,
  #   .data$PRECTOTCORR, .data$ALLSKY_SFC_SW_DWN, .data$T2M
  # )
}
