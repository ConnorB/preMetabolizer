#' Plot stream metabolism input data
#'
#' Creates a five-panel time-series plot of dissolved oxygen, dissolved oxygen
#' saturation, water depth, water temperature, and photosynthetically active
#' radiation. The input must use the column names returned by common
#' streamMetabolizer workflows.
#'
#' @param data A data frame with `solar.time`, `DO.obs`, `DO.sat`, `depth`,
#'   `temp.water`, and `light` columns. The measurement columns must be numeric.
#'
#' @return A ggplot object. The dissolved oxygen saturation percentage is
#'   calculated as `100 * DO.obs / DO.sat`; values where `DO.sat` is zero are
#'   shown as missing.
#'
#' @examples
#' data <- data.frame(
#'   solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:2 * 3600,
#'   DO.obs = c(8, 8.2, 8.4),
#'   DO.sat = c(9, 9.1, 9.2),
#'   depth = c(0.4, 0.4, 0.4),
#'   temp.water = c(18, 18.2, 18.4),
#'   light = c(0, 100, 500)
#' )
#' plot_metab_data(data)
#'
#' @export
plot_metab_data <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  required <- c(
    "solar.time",
    "DO.obs",
    "DO.sat",
    "depth",
    "temp.water",
    "light"
  )
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg data} is missing required columns: {toString(missing)}."
    )
  }

  measurement_cols <- setdiff(required, "solar.time")
  non_numeric <- measurement_cols[
    !vapply(
      data[measurement_cols],
      is.numeric,
      logical(1)
    )
  ]
  if (length(non_numeric) > 0) {
    cli::cli_abort(
      "{.arg data} measurement columns must be numeric: {toString(non_numeric)}."
    )
  }

  panel_labels <- c(
    DO_mgL = "atop(~DO, (mg/L))",
    DO_pctsat = "atop(~DO, ('% Sat'))",
    depth = "atop(~Depth, (m))",
    temp_water = "atop(~Temp, (degree*C))",
    light = "atop(~PAR, (mu*mol~m^{-2}~s^{-1}))"
  )
  variable_labels <- c(
    DO.obs = "DO[obs]",
    DO.sat = "DO[sat]",
    DO.pctsat = "DO['% Sat']",
    depth = "Depth",
    temp.water = "Water~Temp",
    light = "Light"
  )

  data |>
    dplyr::mutate(
      DO.pctsat = dplyr::if_else(
        .data$DO.sat == 0,
        NA_real_,
        100 * .data$DO.obs / .data$DO.sat
      )
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(c(
        "DO.obs",
        "DO.sat",
        "DO.pctsat",
        "depth",
        "temp.water",
        "light"
      )),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      panel = dplyr::case_when(
        .data$variable %in% c("DO.obs", "DO.sat") ~ "DO_mgL",
        .data$variable == "DO.pctsat" ~ "DO_pctsat",
        .data$variable == "depth" ~ "depth",
        .data$variable == "temp.water" ~ "temp_water",
        .data$variable == "light" ~ "light"
      ),
      panel = factor(
        .data$panel,
        levels = c("DO_mgL", "DO_pctsat", "depth", "temp_water", "light")
      ),
      variable = factor(
        .data$variable,
        levels = c(
          "DO.obs",
          "DO.sat",
          "DO.pctsat",
          "depth",
          "temp.water",
          "light"
        )
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      .data$solar.time,
      .data$value,
      color = .data$variable
    )) +
    ggplot2::geom_line(linewidth = 0.3) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$panel),
      scales = "free_y",
      labeller = ggplot2::as_labeller(panel_labels, ggplot2::label_parsed)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        DO.obs = "#0072B2",
        DO.sat = "#56B4E9",
        DO.pctsat = "#009E73",
        depth = "#333333",
        temp.water = "#E8000D",
        light = "#F2A900"
      ),
      breaks = c("DO.obs", "DO.sat"),
      labels = \(x) parse(text = variable_labels[x])
    ) +
    ggplot2::labs(
      x = "Solar time",
      y = NULL,
      color = NULL
    ) +
    ggthemes::theme_few() +
    ggplot2::theme(legend.position = "bottom")
}
