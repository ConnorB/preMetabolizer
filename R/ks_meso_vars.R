#' Fetch Kansas Mesonet variable metadata
#'
#' Retrieves variable metadata from the Kansas Mesonet REST variables page and
#' returns a tidy table of available variables, including their CSV headings,
#' original variable names, cleaned variable names, units, and descriptions.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A tibble with one row per Kansas Mesonet variable and the columns:
#' \describe{
#'   \item{csv_heading}{The variable heading used in Mesonet CSV output.}
#'   \item{var}{The original variable name from the Mesonet metadata.}
#'   \item{tidy_name}{A cleaned, snake_case version of `var`.}
#'   \item{units}{The measurement units for the variable.}
#'   \item{desc}{A description of the variable.}
#' }
#'
#' @details
#' The function downloads the Kansas Mesonet variables page, extracts the embedded
#' JavaScript metadata array, parses variable fields, and standardizes variable
#' names into `tidy_name`. Kansas Mesonet data are preliminary and subject to
#' revision. Cite the Kansas Mesonet when sharing, publishing, or otherwise
#' disseminating data accessed with this function. A suggested citation format
#' is: Kansas Mesonet, year: webpage title. Accessed date, webpage URL. Review
#' the Kansas Mesonet data usage policy before automated use; automated page
#' scraping or data ingesting without written consent is not permitted.
#'
#' @references
#' Kansas Mesonet data usage policy:
#' \url{https://mesonet.k-state.edu/about/usage/}
#'
#' @examples
#' \dontrun{
#' vars <- ks_meso_vars()
#' vars
#' }
#'
#' @export
ks_meso_vars <- function() {
  extract_js_array <- function(x, marker = "jsonSchema = ") {
    start_marker <- regexpr(marker, x, fixed = TRUE)

    if (start_marker < 0) {
      cli::cli_abort("Could not find {.code jsonSchema} marker.")
    }

    start <- start_marker + attr(start_marker, "match.length")
    open <- regexpr("\\[", substring(x, start), perl = TRUE)

    if (open < 0) {
      cli::cli_abort("Could not find opening {.code [}.")
    }

    start <- start + open - 1L
    chars <- strsplit(substring(x, start), "", fixed = TRUE)[[1]]

    depth <- 0L
    in_string <- FALSE
    escaped <- FALSE

    for (i in seq_along(chars)) {
      ch <- chars[[i]]

      if (in_string) {
        if (escaped) {
          escaped <- FALSE
        } else if (ch == "\\") {
          escaped <- TRUE
        } else if (ch == "\"") {
          in_string <- FALSE
        }
      } else {
        if (ch == "\"") {
          in_string <- TRUE
        } else if (ch == "[") {
          depth <- depth + 1L
        } else if (ch == "]") {
          depth <- depth - 1L

          if (depth == 0L) {
            return(substring(x, start, start + i - 1L))
          }
        }
      }
    }

    cli::cli_abort("Could not find matching closing {.code ]}.")
  }

  extract_field <- function(x, field) {
    pattern <- paste0('"', field, '"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)"')
    m <- regexec(pattern, x, perl = TRUE)
    out <- regmatches(x, m)[[1]]

    if (length(out) < 2) {
      NA_character_
    } else {
      gsub('\\"', '"', out[2], fixed = TRUE)
    }
  }

  html <- httr2::request("https://mesonet.k-state.edu/rest/variables/") |>
    http_req_perform() |>
    httr2::resp_body_string()

  json_str <- html |>
    extract_js_array()

  matches <- gregexpr(
    '\\{\\s*"[^"]+"\\s*:\\s*\\{[^{}]*\\}\\s*\\}',
    json_str,
    perl = TRUE
  )

  items <- regmatches(json_str, matches)[[1]]

  if (length(items) == 0) {
    cli::cli_abort("No variable metadata items were found.")
  }

  clean_name <- function(x) {
    x |>
      (\(z) {
        gsub(
          "([a-z0-9])([A-Z])",
          "\\1_\\2",
          x = z,
          perl = TRUE
        )
      })() |>
      (\(z) {
        gsub(
          "[^A-Za-z0-9]+",
          "_",
          x = z,
          perl = TRUE
        )
      })() |>
      tolower() |>
      (\(z) {
        gsub(
          "^_+|_+$",
          "",
          x = z,
          perl = TRUE
        )
      })() |>
      (\(z) {
        gsub(
          "_+",
          "_",
          x = z,
          perl = TRUE
        )
      })()
  }

  tbl <- tibble::tibble(item = items) |>
    dplyr::mutate(
      csv_heading = sub(
        '^\\{\\s*"([^"]+)".*$',
        "\\1",
        .data$item,
        perl = TRUE
      ),
      var = vapply(.data$item, extract_field, character(1), field = "var"),
      tidy_name = clean_name(.data$var),
      units = vapply(.data$item, extract_field, character(1), field = "units"),
      desc = vapply(.data$item, extract_field, character(1), field = "desc")
    ) |>
    dplyr::distinct() |>
    dplyr::select(
      "csv_heading",
      "var",
      "tidy_name",
      "units",
      "desc"
    )

  tbl
}
