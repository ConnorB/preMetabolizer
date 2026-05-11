test_that("get_season returns correct seasons for mid-season dates", {
  result <- get_season(c(
    "2024-01-15",
    "2024-04-15",
    "2024-07-04",
    "2024-10-15"
  ))
  expect_equal(as.character(result), c("Winter", "Spring", "Summer", "Autumn"))
  expect_s3_class(result, "ordered")
  expect_equal(levels(result), c("Spring", "Summer", "Autumn", "Winter"))
})

test_that("get_season handles winter solstice boundary", {
  expect_equal(as.character(get_season("2024-12-21")), "Winter")
  expect_equal(as.character(get_season("2024-12-20")), "Autumn")
})

test_that("get_season uses per-year equinox dates", {
  # 2025 March equinox is on 2025-03-20 09:01 UTC; 2026 is on 2026-03-20 14:46.
  expect_equal(as.character(get_season("2025-03-19")), "Winter")
  expect_equal(as.character(get_season("2025-03-20")), "Spring")
  expect_equal(as.character(get_season("2026-03-20")), "Spring")
})

test_that("get_season accepts Date objects", {
  expect_equal(as.character(get_season(as.Date("2024-07-04"))), "Summer")
})

test_that("get_season is vectorized and preserves length", {
  result <- get_season(c(
    "2024-01-01",
    "2024-04-01",
    "2024-07-01",
    "2024-10-01"
  ))
  expect_length(result, 4)
})

test_that("get_season propagates NA", {
  result <- get_season(as.Date(c("2024-07-04", NA)))
  expect_equal(as.character(result), c("Summer", NA))
})

test_that("get_season supports southern hemisphere", {
  result <- get_season(
    c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"),
    hemisphere = "south"
  )
  expect_equal(as.character(result), c("Summer", "Autumn", "Winter", "Spring"))
})

test_that("get_season supports custom labels", {
  expect_equal(
    as.character(get_season("2024-10-15", labels = c(autumn = "Fall"))),
    "Fall"
  )

  full <- get_season(
    c("2024-01-15", "2024-04-15", "2024-07-15", "2024-10-15"),
    labels = c(
      spring = "Printemps",
      summer = "Ete",
      autumn = "Automne",
      winter = "Hiver"
    )
  )
  expect_equal(
    as.character(full),
    c("Hiver", "Printemps", "Ete", "Automne")
  )
  expect_equal(levels(full), c("Printemps", "Ete", "Automne", "Hiver"))
})

test_that("get_season errors on invalid input", {
  expect_snapshot(error = TRUE, get_season("not-a-date"))
})

test_that("get_season errors on invalid labels", {
  expect_snapshot(error = TRUE, get_season("2024-07-15", labels = "Summer"))
  expect_snapshot(
    error = TRUE,
    get_season("2024-07-15", labels = c(fall = "Fall"))
  )
})
