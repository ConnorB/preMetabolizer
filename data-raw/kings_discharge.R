# data-raw/kings_discharge.R
# Produces data/kings_discharge.rda
#
# Retrieve daily USGS water data for Kings Creek and save it
# as package data for use in analyses, examples, and vignettes.
#
# Monitoring location:
# USGS-06879650 = Kings Creek near Manhattan, Kansas
#
# Parameter codes:
# 00060 = discharge (streamflow)
# 00065 = gage height
# 00010 = water temperature
#
# Date range:
# Water year 2025 (2024-10-01 through 2025-09-30)

kings_discharge <- dataRetrieval::read_waterdata_daily(
  monitoring_location_id = "USGS-06879650",

  # Request multiple hydrologic variables in a single query.
  parameter_code = c("00060", "00065", "00010"),

  # Start and end dates for the requested time series.
  time = c("2024-10-01", "2025-09-30"),

  # Exclude spatial geometry columns to keep the exported
  # package dataset compact and lightweight.
  skipGeometry = TRUE,

  # Retain only columns needed for downstream examples
  # and analyses included in the package.
  properties = c(
    "monitoring_location_id",
    "time",
    "value",
    "parameter_code",
    "qualifier"
  )
)

# Remove retrieval metadata attached by dataRetrieval.
#
# The returned object includes request-related attributes
# (for example, an httr2_request object) that are useful
# interactively but can unnecessarily pull package namespaces
# into the lazyload database when the dataset is stored in
# an R package.
strip_retrieval_metadata <- function(x) {
  attr(x, "httr2_request") <- NULL
  attr(x, "request") <- NULL
  attr(x, "queryTime") <- NULL
  x
}

# Strip retrieval metadata before saving package data.
kings_discharge <- kings_discharge |>
  strip_retrieval_metadata()

# Save dataset to data/kings_discharge.rda.
#
# overwrite = TRUE replaces any existing version of the
# dataset during package development.
usethis::use_data(
  kings_discharge,
  overwrite = TRUE
)
