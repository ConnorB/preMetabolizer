#' Plot stream metabolism input data
#'
#' Creates a time-series plot of the selected input variables, chosen from
#' dissolved oxygen, dissolved oxygen saturation, water depth, water
#' temperature, and photosynthetically active radiation. The input must use the
#' column names returned by common streamMetabolizer workflows.
#'
#' @param data A data frame or tibble with a `solar.time` column and the
#'   measurement columns named in `cols`. The measurement columns must be
#'   numeric.
#' @param cols Character vector of measurement columns to plot. Any subset of
#'   `"DO.obs"`, `"DO.sat"`, `"depth"`, `"temp.water"`, and `"light"`. Defaults
#'   to all five. The dissolved oxygen saturation percentage panel is shown when
#'   both `"DO.obs"` and `"DO.sat"` are selected.
#'
#' @return A ggplot object. The dissolved oxygen saturation percentage is
#'   calculated as `100 * DO.obs / DO.sat`; values where `DO.sat` is zero are
#'   shown as missing.
#'
#' @examples
#' hours <- 0:47
#' data <- tibble::tibble(
#'   solar.time = as.POSIXct("2024-06-01", tz = "UTC") + hours * 3600,
#'   DO.obs = 8 + 1.5 * sin((hours - 10) / 24 * 2 * pi),
#'   DO.sat = 9 - 0.2 * sin((hours - 9) / 24 * 2 * pi),
#'   depth = 0.4,
#'   temp.water = 18 + 2 * sin((hours - 9) / 24 * 2 * pi),
#'   light = pmax(0, sin((hours - 6) / 12 * pi)) * 1500
#' )
#' plot_metab_data(data)
#' plot_metab_data(data, cols = c("DO.obs", "temp.water"))
#'
#' @export
plot_metab_data <- function(
  data,
  cols = c("DO.obs", "DO.sat", "depth", "temp.water", "light")
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  valid_cols <- c("DO.obs", "DO.sat", "depth", "temp.water", "light")
  unknown <- setdiff(cols, valid_cols)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "{.arg cols} contains unknown columns: {toString(unknown)}.",
      "i" = "Valid columns are: {toString(valid_cols)}."
    ))
  }
  if (length(cols) == 0) {
    cli::cli_abort("{.arg cols} must name at least one column.")
  }
  required <- c("solar.time", cols)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg data} is missing required columns: {toString(missing)}."
    )
  }
  non_numeric <- cols[
    !vapply(
      data[cols],
      is.numeric,
      logical(1)
    )
  ]
  if (length(non_numeric) > 0) {
    cli::cli_abort(
      "{.arg data} measurement columns must be numeric: {toString(non_numeric)}."
    )
  }
  plot_vars <- tibble::tribble(
    ~variable    , ~variable_label , ~color    , ~panel       , ~panel_label                         ,
    "DO.obs"     , "DO[obs]"       , "#0072B2" , "DO_mgL"     , "atop(~DO, (mg/L))"                  ,
    "DO.sat"     , "DO[sat]"       , "#56B4E9" , "DO_mgL"     , "atop(~DO, (mg/L))"                  ,
    "DO.pctsat"  , "DO['% Sat']"   , "#009E73" , "DO_pctsat"  , "atop(~DO, ('% Sat'))"               ,
    "depth"      , "Depth"         , "#333333" , "depth"      , "atop(~Depth, (m))"                  ,
    "temp.water" , "Water~Temp"    , "#E8000D" , "temp_water" , "atop(~Temp, (degree*C))"            ,
    "light"      , "Light"         , "#F2A900" , "light"      , "atop(~PAR, (mu*mol~m^{-2}~s^{-1}))"
  )
  show_pctsat <- all(c("DO.obs", "DO.sat") %in% cols)
  keep_vars <- c(cols, if (show_pctsat) "DO.pctsat")
  plot_vars <- plot_vars |>
    dplyr::filter(.data$variable %in% keep_vars)
  panel_labels <- plot_vars |>
    dplyr::distinct(.data$panel, .data$panel_label) |>
    tibble::deframe()
  variable_labels <- plot_vars |>
    dplyr::select("variable", "variable_label") |>
    tibble::deframe()
  variable_colors <- plot_vars |>
    dplyr::select("variable", "color") |>
    tibble::deframe()
  legend_breaks <- intersect(c("DO.obs", "DO.sat"), plot_vars$variable)
  data |>
    dplyr::mutate(
      DO.pctsat = if (show_pctsat) {
        dplyr::if_else(
          .data$DO.sat == 0,
          NA_real_,
          100 * .data$DO.obs / .data$DO.sat
        )
      } else {
        NULL
      }
    ) |>
    tidyr::pivot_longer(
      dplyr::all_of(plot_vars$variable),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::left_join(
      plot_vars |> dplyr::select("variable", "panel"),
      by = "variable"
    ) |>
    dplyr::mutate(
      panel = factor(.data$panel, levels = unique(plot_vars$panel)),
      variable = factor(.data$variable, levels = plot_vars$variable)
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
      values = variable_colors,
      breaks = legend_breaks,
      labels = \(x) parse(text = variable_labels[x])
    ) +
    ggplot2::labs(
      x = "Solar time",
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
