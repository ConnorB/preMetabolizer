skip_if_not_installed("sf")
skip_if_not_installed("withr")

make_point_sf <- function() {
  sf::st_as_sf(
    data.frame(id = 1L, x = -96.6, y = 39.1),
    coords = c("x", "y"),
    crs = 4326
  )
}

test_that("read_shp reads a .shp file by path", {
  dir <- withr::local_tempdir()
  shp_path <- file.path(dir, "site.shp")
  sf::st_write(make_point_sf(), shp_path, quiet = TRUE)

  result <- read_shp(shp_path)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
})

test_that("read_shp reads a directory containing a single layer", {
  dir <- withr::local_tempdir()
  sf::st_write(make_point_sf(), file.path(dir, "site.shp"), quiet = TRUE)

  result <- read_shp(dir)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
})

test_that("read_shp reads a zipped shapefile", {
  dir <- withr::local_tempdir()
  sf::st_write(make_point_sf(), file.path(dir, "site.shp"), quiet = TRUE)
  zip_path <- file.path(dir, "site.zip")
  withr::with_dir(
    dir,
    utils::zip(zip_path, list.files(pattern = "^site\\."), flags = "-q")
  )

  result <- read_shp(zip_path)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
})

scrub_tempdir <- function(x) {
  gsub(
    paste0(normalizePath(tempdir(), winslash = "/"), "/[^'\"[:space:]]+"),
    "<tempdir>",
    gsub("\\\\", "/", x)
  )
}

test_that("read_shp warns when a directory contains multiple layers", {
  dir <- withr::local_tempdir()
  sf::st_write(make_point_sf(), file.path(dir, "a.shp"), quiet = TRUE)
  sf::st_write(make_point_sf(), file.path(dir, "b.shp"), quiet = TRUE)

  expect_snapshot(
    {
      result <- read_shp(dir)
    },
    transform = scrub_tempdir
  )
  expect_s3_class(result, "sf")
})

test_that("read_shp errors when no layers are found", {
  dir <- withr::local_tempdir()
  expect_snapshot(
    error = TRUE,
    read_shp(dir),
    transform = scrub_tempdir
  )
})
