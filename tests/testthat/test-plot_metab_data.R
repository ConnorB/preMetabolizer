metab_plot_data <- function() {
  tibble::tibble(
    solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:2 * 3600,
    DO.obs = c(8, 8.2, 8.4),
    DO.sat = c(9, 0, 9.2),
    depth = c(0.4, 0.4, 0.4),
    temp.water = c(18, 18.2, 18.4),
    light = c(0, 100, 500)
  )
}

test_that("plot_metab_data creates a five-panel plot", {
  plot <- plot_metab_data(metab_plot_data())

  expect_s3_class(plot, "ggplot")
  expect_equal(
    levels(plot$data$panel),
    c(
      "DO_mgL",
      "DO_pctsat",
      "depth",
      "temp_water",
      "light"
    )
  )
  expect_equal(
    plot$data$value[plot$data$variable == "DO.pctsat"],
    c(100 * 8 / 9, NA_real_, 100 * 8.4 / 9.2)
  )
})

test_that("plot_metab_data validates its input", {
  expect_snapshot(error = TRUE, {
    plot_metab_data(1)
  })

  expect_snapshot(error = TRUE, {
    plot_metab_data(metab_plot_data()[c("solar.time", "DO.obs")])
  })

  data <- metab_plot_data()
  data$light <- as.character(data$light)
  expect_snapshot(error = TRUE, {
    plot_metab_data(data)
  })
})
