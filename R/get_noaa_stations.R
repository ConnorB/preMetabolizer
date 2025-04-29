#' Get NOAA Station Information
#'
#' Downloads, processes, and optionally cleans NOAA station metadata from the Meteorological Station Historical Repository (MSHR).
#'
#' @param state Optional character string specifying a two-letter U.S. state code to filter stations.
#' @param clean Logical. If `TRUE` (default), returns processed and cleaned data; if `FALSE`, returns raw data.
#' @param debug Logical. If `TRUE` (default), outputs debug messages during data download and processing.
#'
#' @return A data frame containing NOAA station metadata. Columns include station identifiers, names, location (latitude, longitude), elevation, operational dates, and more.
#'
#' @details
#' This function retrieves and processes data from NOAA's Meteorological Station Historical Repository (MSHR). The data include detailed information about meteorological stations, including their geographic coordinates, elevation, operational status, and identifiers across various systems (e.g., GHCND, WBAN, FAA).
#'
#' If the data file already exists in a cached location and is up-to-date, the cached data is loaded to improve performance.
#'
#' When `clean = TRUE`, the function processes the raw data by:
#' \itemize{
#'   \item Parsing dates and filtering invalid or missing coordinate values.
#'   \item Filtering out balloon platforms.
#'   \item Aggregating station data to consolidate records by station identifier (`GHCND_ID`).
#' }
#'
#' If a state code is provided via the `state` parameter, the returned data will be limited to stations located within that state.
#'
#' @examples
#' \dontrun{
#' # Retrieve and clean all NOAA stations
#' all_stations <- get_noaa_stations()
#'
#' # Retrieve raw data for stations in California
#' ca_stations_raw <- get_noaa_stations(state = "CA", clean = FALSE)
#'
#' # Retrieve cleaned data for stations in Texas with debug messages
#' tx_stations <- get_noaa_stations(state = "TX", debug = TRUE)
#' }
#'
#' @export
get_noaa_stations <- function(state = NULL, clean = TRUE, debug = TRUE) {
  BEGIN_DATE <- END_DATE <- LAT_DEC <- LON_DEC <- PLATFORM <- STATE_PROV <- GHCND_ID <- NULL
  ..cols_to_keep <- NULL

  mshr_url <- "https://www.ncei.noaa.gov/access/homr/file/mshr_enhanced.txt.zip"

  mshr_zip <- file.path(noaa_cache(), "mshr_enhanced.txt.zip")
  mshr_txt <- file.path(noaa_cache(), "mshr_enhanced.txt")
  mshr_rds <- file.path(noaa_cache(), "mshr_enhanced.rds")

  # Validate state parameter if provided
  if (!is.null(state)) {
    if (!is.character(state) || length(state) != 1) {
      stop("state must be a single character string")
    }
    state <- toupper(trimws(state))
    if (nchar(state) != 2) {
      stop("state must be a two-letter state code")
    }
  }

  # Define column specs
  spec <- readr::fwf_positions(
    start = c(
      1,   22,  33,  42,  51,  72,  93,  114, 135, 156,
      177, 198, 219, 240, 261, 362, 393, 494, 525, 626,
      727, 738, 779, 790, 841, 844, 847, 948, 979, 990,
      1031,1052,1093,1114,1155,1176,1217,1238,1279,1300,
      1321,1342,1353,1416,1433,1474,1575,1596,1602,1633,
      1664,1765,1786,1807
    ),
    end = c(
      20,  31,  40,  49,  70,  91,  112, 133, 154, 175,
      196, 217, 238, 259, 360, 391, 492, 523, 624, 725,
      736, 777, 788, 839, 842, 845, 946, 977, 988, 1029,
      1050,1091,1112,1153,1174,1215,1236,1277,1298,1319,
      1340,1351,1414,1431,1472,1573,1594,1600,1631,1662,
      1763,1784,1805,1826
    ),
    col_names = c(
      "SOURCE_ID", "SOURCE", "BEGIN_DATE", "END_DATE", "STATION_STATUS",
      "NCDCSTN_ID", "ICAO_ID", "WBAN_ID", "FAA_ID", "NWSLI_ID",
      "WMO_ID", "COOP_ID", "TRANSMITTAL_ID", "GHCND_ID", "NAME_PRINCIPAL",
      "NAME_PRINCIPAL_SHORT", "NAME_COOP", "NAME_COOP_SHORT",
      "NAME_PUBLICATION", "NAME_ALIAS", "NWS_CLIM_DIV", "NWS_CLIM_DIV_NAME",
      "STATE_PROV", "COUNTY", "NWS_ST_CODE", "FIPS_COUNTRY_CODE",
      "FIPS_COUNTRY_NAME", "NWS_REGION", "NWS_WFO", "ELEV_GROUND",
      "ELEV_GROUND_UNIT", "ELEV_BAROM", "ELEV_BAROM_UNIT", "ELEV_AIR",
      "ELEV_AIR_UNIT", "ELEV_ZERODAT", "ELEV_ZERODAT_UNIT", "ELEV_UNK",
      "ELEV_UNK_UNIT", "LAT_DEC", "LON_DEC", "LAT_LON_PRECISION",
      "RELOCATION", "UTC_OFFSET", "OBS_ENV", "PLATFORM", "GHCNMLT_ID",
      "COUNTY_FIPS_CODE", "DATUM_HORIZONTAL", "DATUM_VERTICAL",
      "LAT_LON_SOURCE", "IGRA_ID", "HPD_ID", "GHCNH_ID"
    )
  )

  # Process data function
  process_data <- memoise::memoise(function(df, state, debug) {
    if (debug) message("Processing data...")

    cols_to_keep <- c(
      "GHCND_ID", "NCDCSTN_ID", "WBAN_ID", "FAA_ID", "COOP_ID", "BEGIN_DATE", "END_DATE",
      "NAME_PRINCIPAL", "NAME_ALIAS", "STATE_PROV", "FIPS_COUNTRY_CODE",
      "FIPS_COUNTRY_NAME", "ELEV_GROUND", "ELEV_GROUND_UNIT",
      "ELEV_BAROM", "ELEV_BAROM_UNIT", "ELEV_AIR", "ELEV_AIR_UNIT",
      "ELEV_ZERODAT", "ELEV_ZERODAT_UNIT", "ELEV_UNK", "ELEV_UNK_UNIT",
      "LAT_DEC", "LON_DEC", "UTC_OFFSET", "PLATFORM", "GHCNMLT_ID",
      "GHCNH_ID"
    )

    dt <- data.table::as.data.table(df)

    dt <- dt[BEGIN_DATE == "00010101", BEGIN_DATE := NA_character_
    ][END_DATE == "99991231", END_DATE := format(Sys.Date(), "%Y%m%d")
    ][, c("BEGIN_DATE", "END_DATE") := list(
      as.Date(BEGIN_DATE, format = "%Y%m%d"),
      as.Date(END_DATE, format = "%Y%m%d")
    )][, c("LAT_DEC", "LON_DEC") := list(
      as.numeric(gsub("[^0-9.-]", "", LAT_DEC)),
      as.numeric(gsub("[^0-9.-]", "", LON_DEC))
    )][!is.na(LAT_DEC) &
         !is.na(LON_DEC) &
         abs(LAT_DEC) >= 0.1 &
         abs(LAT_DEC) <= 90 &
         abs(LON_DEC) >= 0.1 &
         abs(LON_DEC) <= 180 &
         PLATFORM != "UPPERAIR,BALLOON"
    ]

    if (!is.null(state)) {
      dt <- dt[toupper(trimws(STATE_PROV)) == state]
      if (nrow(dt) == 0) {
        warning(sprintf("No stations found for state code '%s'", state))
      }
    }

    id_cols <- "GHCND_ID"
    special_cols <- c("BEGIN_DATE", "END_DATE")
    other_cols <- setdiff(cols_to_keep, c(id_cols, special_cols))

    result <- dt[order(GHCND_ID), {
      c(
        lapply(.SD[1], identity),
        list(
          BEGIN_DATE = if (all(is.na(BEGIN_DATE))) NA_real_ else min(BEGIN_DATE, na.rm = TRUE),
          END_DATE = if (all(is.na(END_DATE))) NA_real_ else max(END_DATE, na.rm = TRUE)
        )
      )
    }, by = id_cols, .SDcols = other_cols]

    result <- result[, ..cols_to_keep]
    result <- result[, colSums(!is.na(result)) > 0, with = FALSE]

    as.data.frame(result)
  }, cache = memoise::cache_filesystem(getOption("preMetabolizer.noaa_cache")))

  # Check if RDS exists and is up to date
  needs_update <- TRUE
  if (file.exists(mshr_rds)) {
    remote_time <- get_remote_mtime(mshr_url)
    if (!is.null(remote_time)) {
      rds_time <- file.info(mshr_rds)$mtime
      needs_update <- remote_time > rds_time
    }
    if (!needs_update) {
      if (debug) message("Loading cached RDS file")
      raw_data <- readRDS(mshr_rds)
      return(if (clean) process_data(raw_data, state, debug) else raw_data)
    }
  }

  # If RDS doesn't exist or needs update, download and process the data
  tryCatch({
    if (debug) message("Downloading and processing data...")

    # Download zip file
    utils::download.file(mshr_url, mshr_zip, mode = "wb", quiet = !debug)

    # Unzip file
    utils::unzip(mshr_zip, exdir = dirname(mshr_txt))
    mshr_txt <- list.files(noaa_cache(), pattern = "\\.txt$", full.names = TRUE)[[1]]
    if (debug) message("Reading file: ", mshr_txt)

    # Read the file using read_fwf
    raw_data <- readr::read_fwf(
      mshr_txt,
      readr::fwf_positions(
        start = spec$begin,
        end = spec$end,
        col_names = spec$col_names
      ),
      col_types = readr::cols(.default = "c"),
      progress = debug,
      trim_ws = TRUE,
      na = c("", "NA")
    )

    # Save raw data to RDS
    saveRDS(raw_data, mshr_rds)

    # Clean up temporary files
    unlink(mshr_zip)
    unlink(mshr_txt)

    # Return either raw or processed data based on clean parameter
    if (!clean) {
      # If state is provided but clean=FALSE, still filter by state
      if (!is.null(state)) {
        raw_data <- raw_data[toupper(trimws(raw_data$STATE_PROV)) == state, ]
        if (nrow(raw_data) == 0) {
          warning(sprintf("No stations found for state code '%s'", state))
        }
      }
      return(raw_data)
    } else {
      return(process_data(raw_data, state, debug))
    }

  }, error = function(e) {
    # Clean up any temporary files in case of error
    unlink(mshr_zip)
    unlink(mshr_txt)
    stop("Processing failed: ", e$message)
  })
}
