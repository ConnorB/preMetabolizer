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
