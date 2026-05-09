# Get NOAA Station Information

Downloads, processes, and optionally cleans NOAA station metadata from
the Meteorological Station Historical Repository (MSHR).

## Usage

``` r
get_noaa_stations(state = NULL, clean = TRUE, debug = TRUE)
```

## Arguments

- state:

  Optional character string specifying a two-letter U.S. state code to
  filter stations.

- clean:

  Logical. If `TRUE` (default), returns processed and cleaned data; if
  `FALSE`, returns raw data.

- debug:

  Logical. If `TRUE` (default), outputs debug messages during data
  download and processing.

## Value

A data frame containing NOAA station metadata. Columns include station
identifiers, names, location (latitude, longitude), elevation,
operational dates, and more.

## Details

This function retrieves and processes data from NOAA's Meteorological
Station Historical Repository (MSHR). The data include detailed
information about meteorological stations, including their geographic
coordinates, elevation, operational status, and identifiers across
various systems (e.g., GHCND, WBAN, FAA).

If the data file already exists in a cached location and is up-to-date,
the cached data is loaded to improve performance.

When `clean = TRUE`, the function processes the raw data by:

- Parsing dates and filtering invalid or missing coordinate values.

- Filtering out balloon platforms.

- Aggregating station data to consolidate records by station identifier
  (`GHCND_ID`).

If a state code is provided via the `state` parameter, the returned data
will be limited to stations located within that state.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve and clean all NOAA stations
all_stations <- get_noaa_stations()

# Retrieve raw data for stations in California
ca_stations_raw <- get_noaa_stations(state = "CA", clean = FALSE)

# Retrieve cleaned data for stations in Texas with debug messages
tx_stations <- get_noaa_stations(state = "TX", debug = TRUE)
} # }
```
