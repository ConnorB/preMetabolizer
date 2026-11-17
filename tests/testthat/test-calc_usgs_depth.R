test_that("calc_usgs_depth fits USGS channel measurements", {
  requested_id <- NULL
  measurements <- data.frame(
    field_visit_id = c("a", "b", "b", "c"),
    measurement_number = c("1", "2", "2", "3"),
    channel_flow = c(1, 2, 2, 9),
    channel_flow_unit = "ft^3/s",
    channel_width = c(2, 1, 1, 2),
    channel_width_unit = "ft",
    channel_area = c(2, 2, 2, 6),
    channel_area_unit = "ft^2"
  )

  local_mocked_bindings(
    read_usgs_channel = function(monitoring_location_id, ...) {
      requested_id <<- monitoring_location_id
      measurements
    }
  )

  expect_equal(
    calc_usgs_depth("01234567", c(1, 4, 9)),
    c(1, 2, 3) * 0.3048
  )
  expect_equal(requested_id, "USGS-01234567")
})

test_that("calc_usgs_depth validates its inputs and calibration data", {
  expect_snapshot(error = TRUE, {
    calc_usgs_depth(1234567, 1)
  })
  expect_snapshot(error = TRUE, {
    calc_usgs_depth("not-a-gage", 1)
  })
  expect_snapshot(error = TRUE, {
    calc_usgs_depth("01234567", 0)
  })

  local_mocked_bindings(
    read_usgs_channel = function(...) {
      data.frame(
        field_visit_id = "a",
        measurement_number = "1",
        channel_flow = 1,
        channel_flow_unit = "ft^3/s",
        channel_width = 1,
        channel_width_unit = "ft",
        channel_area = 1,
        channel_area_unit = "ft^2"
      )
    }
  )

  expect_snapshot(error = TRUE, {
    calc_usgs_depth("01234567", 1)
  })
})
