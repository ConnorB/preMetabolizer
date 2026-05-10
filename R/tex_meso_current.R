#' Get current TexMesonet data
#'
#' Retrieves the most recent observation from each TWDB TexMesonet station.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @return A tibble containing the most recent station observations. The API
#'   returns availability by station, so some measurement columns may contain
#'   missing values. The returned tibble has a `"units"` attribute containing
#'   units reported by the API.
#'
#' @details
#' Current data are near-real-time observations. TexMesonet reports values in
#' the units returned by the API, available with `attr(x, "units")`.
#'
#' @references
#' TexMesonet APIs:
#' \url{https://www.texmesonet.org/Apis}
#'
#' @examples
#' \dontrun{
#' current <- tex_meso_current()
#' attr(current, "units")
#' }
#'
#' @export
tex_meso_current <- function() {
  response <- tex_meso_request("CurrentData") |>
    tex_meso_perform_json(
      "Failed to fetch current TexMesonet data."
    )

  current <- tex_meso_as_tibble(
    response$data,
    numeric_exclude = c("name", "displayId", "recordedTime")
  )

  if ("recordedTime" %in% names(current)) {
    current$recordedTime <- tex_meso_parse_datetime(current$recordedTime)
  }

  attr(current, "units") <- response$units
  current
}
