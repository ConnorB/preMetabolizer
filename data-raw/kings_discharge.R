# data-raw/kings_discharge.R
# Produces data/kings_discharge.rda
# Retrieve daily USGS water data for Kings Creek and save as
# package data for use in analyses, examples, and vignettes.
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
  parameter_code = c("00060", "00065", "00010"),
  time = c("2024-10-01", "2025-09-30"),

  # Exclude spatial geometry from the returned dataset
  # to keep the package dataset lightweight.
  skipGeometry = TRUE,

  # Keep only selected columns needed for the package dataset.
  properties = c(
    "monitoring_location_id",
    "time",
    "value",
    "parameter_code",
    "qualifier"
  )
)

# Save dataset to data/kings_discharge.rda
# overwrite = TRUE replaces any existing version.
usethis::use_data(
  kings_discharge,
  overwrite = TRUE
)
