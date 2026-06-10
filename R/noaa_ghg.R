noaa_ghg_base_url <- "https://www.gml.noaa.gov/webdata/ccgg/trends"

# Native NOAA reporting unit for each gas: CO2 in ppm, CH4 and N2O in ppb, SF6
# in ppt.
noaa_ghg_native_unit <- c(co2 = "ppm", ch4 = "ppb", n2o = "ppb", sf6 = "ppt")

# Mole-fraction magnitude of each supported unit, used to convert between them.
noaa_ghg_unit_scale <- c(ppm = 1e-6, ppb = 1e-9, ppt = 1e-12)

#' Get NOAA global monthly mean greenhouse gas concentrations
#'
#' Downloads the NOAA Global Monitoring Laboratory (GML) globally averaged
#' marine surface monthly mean records for carbon dioxide, methane, nitrous
#' oxide, and sulphur hexafluoride. By default each gas is returned in its
#' standard NOAA reporting unit; set `units` to express every gas on a common
#' scale.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param gas Character vector naming one or more gases to download. Any of
#'   `"co2"`, `"ch4"`, `"n2o"`, and `"sf6"` (case insensitive). Defaults to all
#'   four.
#' @param units Character string giving the output mole-fraction unit, one of
#'   `"ppm"`, `"ppb"`, or `"ppt"` (case insensitive). When `NULL` (the default)
#'   each gas is returned in its standard NOAA reporting unit (CO2 in ppm, CH4
#'   and N2O in ppb, SF6 in ppt); otherwise every gas is converted to the
#'   requested unit.
#' @param quiet Logical. When `TRUE` progress messages are suppressed. Default
#'   `FALSE`.
#'
#' @return A [tibble][tibble::tibble-package] with one row per gas-month and the
#'   columns:
#'   \describe{
#'     \item{`gas`}{Gas name in upper case (`"CO2"`, `"CH4"`, `"N2O"`,
#'       `"SF6"`).}
#'     \item{`date`}{Month of the observation as a `POSIXct`, centred on the
#'       middle of the month (NOAA's decimal date).}
#'     \item{`unit`}{Mole-fraction unit of the value columns (`"ppm"`, `"ppb"`,
#'       or `"ppt"`).}
#'     \item{`average`}{Globally averaged monthly mean mole fraction.}
#'     \item{`average_unc`}{Uncertainty of the monthly mean.}
#'     \item{`trend`}{De-seasonalised trend value.}
#'     \item{`trend_unc`}{Uncertainty of the trend value.}
#'   }
#'   NOAA reports a negative sentinel (`-9.99`) when an uncertainty has not yet
#'   been calculated; these are returned as `NA`.
#'
#' @details
#' NOAA reports each gas in a different native unit (CO2 in ppm, CH4 and N2O in
#' ppb, and SF6 in ppt). By default these native units are preserved and
#' recorded in the `unit` column. When `units` is supplied, every gas is
#' rescaled to that common unit.
#'
#' The data presented for the most recent year are preliminary and subject to
#' change as reference gases are recalibrated.
#'
#' These data are made freely available by NOAA GML. When the data are central
#' to a publication, please cite NOAA GML and consider contacting the data
#' providers, who can advise on appropriate use and acknowledgement.
#'
#' @source <https://gml.noaa.gov/ccgg/trends/>
#'
#' @references
#' Lan, X., Tans, P., & K.W. Thoning: Trends in globally-averaged greenhouse
#' gases determined from NOAA Global Monitoring Laboratory measurements.
#' \url{https://gml.noaa.gov/ccgg/trends/}
#'
#' Dlugokencky, E. J., et al. (1994). The growth rate and distribution of
#' atmospheric methane. \emph{Journal of Geophysical Research}, 99(D8),
#' 17021--17043. \doi{10.1029/94JD01245}
#'
#' @examples
#' \dontrun{
#' # All four gases in their standard units, stacked long
#' ghg <- get_noaa_ghg()
#'
#' # Convert every gas to ppb
#' ghg_ppb <- get_noaa_ghg(units = "ppb")
#'
#' # A single gas
#' co2 <- get_noaa_ghg("co2")
#' }
#'
#' @importFrom rlang .data
#' @export
get_noaa_ghg <- function(
  gas = c("co2", "ch4", "n2o", "sf6"),
  units = NULL,
  quiet = FALSE
) {
  check_character(gas, allow_empty = FALSE, allow_na = FALSE)
  check_bool(quiet)

  if (!is.null(units)) {
    units <- tolower(units)
    units <- rlang::arg_match(units, names(noaa_ghg_unit_scale))
  }

  gas <- unique(tolower(gas))
  valid <- names(noaa_ghg_native_unit)
  bad <- setdiff(gas, valid)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "{.arg gas} must be one of {.val {valid}}.",
      "x" = "Unknown gas{?es}: {.val {bad}}."
    ))
  }

  reqs <- lapply(gas, function(g) {
    cli_inform_if(!quiet, "Requesting NOAA global monthly {toupper(g)} data.")
    noaa_ghg_request(g)
  })

  resps <- http_req_perform_parallel(
    reqs,
    on_error = "return",
    progress = !quiet
  )

  dfs <- lapply(seq_along(resps), function(i) {
    resp <- resps[[i]]
    g <- gas[i]

    if (inherits(resp, "error")) {
      cli::cli_warn(c(
        "Failed to download NOAA {toupper(g)} data.",
        "i" = conditionMessage(resp)
      ))
      return(NULL)
    }

    tryCatch(
      noaa_ghg_parse(httr2::resp_body_string(resp), g, units),
      error = function(e) {
        cli::cli_warn(c(
          "Failed to parse NOAA {toupper(g)} data.",
          "i" = conditionMessage(e)
        ))
        NULL
      }
    )
  })

  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) {
    cli::cli_abort("No NOAA greenhouse gas data could be retrieved.")
  }

  dplyr::bind_rows(dfs)
}

noaa_ghg_request <- function(gas) {
  url <- sprintf("%s/%s/%s_mm_gl.txt", noaa_ghg_base_url, gas, gas)
  httr2::request(url) |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3)
}

noaa_ghg_parse <- function(body, gas, units = NULL) {
  native <- noaa_ghg_native_unit[[gas]]
  out_unit <- if (is.null(units)) native else units
  conv <- noaa_ghg_unit_scale[[native]] / noaa_ghg_unit_scale[[out_unit]]
  label <- toupper(gas)

  readr::read_table(
    I(body),
    comment = "#",
    na = c("", "NA", "-9.9", "-9.99"),
    col_names = c(
      "year",
      "month",
      "decimal_date",
      "average",
      "average_unc",
      "trend",
      "trend_unc"
    ),
    col_types = readr::cols(
      year = readr::col_integer(),
      month = readr::col_integer(),
      decimal_date = readr::col_double(),
      average = readr::col_double(),
      average_unc = readr::col_double(),
      trend = readr::col_double(),
      trend_unc = readr::col_double()
    ),
    progress = FALSE
  ) |>
    dplyr::mutate(
      dplyr::across(
        c("average", "average_unc", "trend", "trend_unc"),
        \(x) x * conv
      ),
      gas = label,
      date = lubridate::date_decimal(.data$decimal_date),
      unit = out_unit,
      .before = 1L
    ) |>
    dplyr::select(
      "gas",
      "date",
      "unit",
      "average",
      "average_unc",
      "trend",
      "trend_unc"
    )
}
