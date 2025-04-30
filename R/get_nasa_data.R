#' Download and process NASA POWER data for multiple sites in parallel
#'
#' @param metadata A dataframe containing site information with columns:
#'   Site, Long, Lat, StartDate, EndDate, Elev_m
#' @return A tibble with columns: Site, dateTime, bp_mbar, PAR.obs
#' @export
#' @examples
#' \dontrun{
#' metadata <- data.frame(
#'   Site = c("Site1", "Site2"),
#'   Long = c(-100, -101),
#'   Lat = c(40, 41),
#'   StartDate = as.Date(c("2023-01-01", "2023-01-01")),
#'   EndDate = as.Date(c("2023-12-31", "2023-12-31")),
#'   Elev_m = c(1000, 1100)
#' )
#' data <- get_nasa_data(metadata)
#' }
get_nasa_data <- function(metadata) {
  metadata |>
    download_site_data_parallel() |>
    process_nasa_data()
}

#' Download NASA POWER data for a single site
#' @param site Site identifier
#' @param long Longitude
#' @param lat Latitude
#' @param start Start date
#' @param end End date
#' @param elev Elevation in meters
#' @return A dataframe with NASA POWER data
#' @keywords internal
#' @importFrom nasapower get_power
fetch_single_site <- function(site, long, lat, start, end, elev) {
  message(sprintf("Downloading data for %s", site))

  data <- nasapower::get_power(
    community = "sb",
    pars = c("PSC", "ALLSKY_SFC_SW_DWN"),
    temporal_api = "hourly",
    lonlat = c(long, lat),
    dates = c(start, end),
    site_elevation = elev,
    time_standard = "UTC"
  )

  data$Site <- site
  data
}

#' Download data for all sites in metadata using parallelization
#' @param metadata Dataframe with site information
#' @return Combined dataframe with all sites' data
#' @keywords internal
#' @importFrom furrr future_pmap
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
download_site_data_parallel <- function(metadata) {
  # Validate required columns
  required_columns <- c("Site", "Long", "Lat", "StartDate", "EndDate", "Elev_m")
  missing_columns <- setdiff(required_columns, colnames(metadata))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in metadata: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # Use future_pmap for parallelized fetching
  furrr::future_pmap(
    list(
      site = metadata$Site,
      long = metadata$Long,
      lat = metadata$Lat,
      start = metadata$StartDate,
      end = metadata$EndDate,
      elev = metadata$Elev_m
    ),
    fetch_single_site,
    .options = furrr::furrr_options(seed = T)
  ) |>
    dplyr::bind_rows()
}

#' Process NASA POWER data
#' @param raw_data Combined raw data from all sites
#' @return Processed tibble with required columns
#' @keywords internal
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
#' @importFrom LakeMetabolizer sw.to.par.base
process_nasa_data <- function(raw_data) {
  required_columns <- c("HR", "YYYYMMDD", "PSC", "ALLSKY_SFC_SW_DWN", "Site")

  if (!all(required_columns %in% colnames(raw_data))) {
    stop(
      "Missing required columns in raw_data: ",
      paste(setdiff(required_columns, colnames(raw_data)), collapse = ", ")
    )
  }

  raw_data |>
    dplyr::mutate(
      Hour = stringr::str_pad(.data$HR, 2, side = "left", pad = "0"),
      dateTime = lubridate::ymd_hms(paste0(.data$YYYYMMDD, .data$Hour, ":00:00")),
      bp_mbar = .data$PSC * 10, # Convert kilopascals to millibar
      #coef Numerical coefficient to convert SW (W/m^2) to PAR  (umol/m^2/sec). Defaults to value from Britton and Dodd (1976).
      PAR.obs = LakeMetabolizer::sw.to.par.base(sw = .data$ALLSKY_SFC_SW_DWN, coef = 2.114)
    ) |>
    dplyr::select(
      .data$Site,
      dateTime = .data$dateTime,
      bp_mbar = .data$bp_mbar,
      PAR.obs = .data$PAR.obs
    ) |>
    tibble::as_tibble()
}
