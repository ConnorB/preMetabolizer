# Query the NCEI Climate Data Online (CDO) Web Services v2 API

These functions wrap the seven endpoints of NOAA's Climate Data Online
Web Services v2 API (`https://www.ncei.noaa.gov/cdo-web/api/v2`). They
retrieve observations (`cdo_data()`), search the station catalog
(`cdo_stations()`), and browse the metadata catalog (`cdo_datasets()`,
`cdo_datacategories()`, `cdo_datatypes()`, `cdo_locationcategories()`,
`cdo_locations()`).

## Usage

``` r
cdo_datasets(
  id = NULL,
  datatypeid = NULL,
  locationid = NULL,
  stationid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_datacategories(
  id = NULL,
  datasetid = NULL,
  locationid = NULL,
  stationid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_datatypes(
  id = NULL,
  datasetid = NULL,
  locationid = NULL,
  stationid = NULL,
  datacategoryid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_locationcategories(
  id = NULL,
  datasetid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_locations(
  id = NULL,
  datasetid = NULL,
  locationcategoryid = NULL,
  datacategoryid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_stations(
  id = NULL,
  datasetid = NULL,
  locationid = NULL,
  datacategoryid = NULL,
  datatypeid = NULL,
  extent = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
)

cdo_data(
  datasetid,
  startdate,
  enddate,
  datatypeid = NULL,
  locationid = NULL,
  stationid = NULL,
  units = "metric",
  sortfield = NULL,
  sortorder = NULL,
  includemetadata = FALSE,
  max_results = Inf
)
```

## Arguments

- id:

  Optional character string. When supplied, fetches a single resource by
  its identifier (e.g., `cdo_datasets("GHCND")`) and returns a named
  list. When `NULL` (default) the list endpoint is queried and a tibble
  is returned.

- startdate, enddate:

  Optional date filters as `Date` objects or `"YYYY-MM-DD"` strings. For
  `cdo_data()` both are required.

- sortfield, sortorder:

  Optional sort controls. `sortfield` is one of `"id"`, `"name"`,
  `"mindate"`, `"maxdate"`, `"datacoverage"`; `sortorder` is `"asc"`
  (default) or `"desc"`.

- max_results:

  Maximum number of rows to return. Defaults to `Inf` (all matching
  results).

- datasetid, datatypeid, locationid, stationid, datacategoryid,
  locationcategoryid:

  Optional character vectors filtering the result set. Multiple values
  are sent as repeated query parameters.

- extent:

  For `cdo_stations()` only. Numeric vector of length 4 giving a
  bounding box as `c(min_lat, min_lon, max_lat, max_lon)`.

- units:

  For `cdo_data()` only. `"metric"` (default) or `"standard"`.

- includemetadata:

  For `cdo_data()` only. Logical; when `FALSE` (default) the API skips
  computing the result-set count, which can noticeably speed up large
  requests.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
for list queries, or a named list for single-resource (`id`-based)
queries. Date-like columns (`mindate`, `maxdate`, `date`) are parsed to
`Date` / `POSIXct` and numeric columns (`latitude`, `longitude`,
`elevation`, `value`, `datacoverage`) are coerced to numeric.

## Authentication

All CDO endpoints require a free API token. Request one at
<https://www.ncdc.noaa.gov/cdo-web/token> and store it in the
`API_NCEI_CDO` environment variable, for example by adding
`API_NCEI_CDO=...` to your `~/.Renviron`.

## Pagination

List endpoints paginate at 1000 results per page. By default each
function transparently walks all pages and returns a single tibble. Use
`max_results` to cap the total number of rows returned.

## Rate limits

Each token is limited to 5 requests per second and 10,000 requests per
day. The 5/s cap is enforced client-side via
[`httr2::req_throttle()`](https://httr2.r-lib.org/reference/req_throttle.html)
(requests block briefly rather than failing). The package also tracks a
session-local counter; once 10,000 successful requests have been made in
the current R session the next call aborts with an error directing you
to
[`cdo_reset_request_count()`](https://connorb.github.io/preMetabolizer/reference/cdo_request_count.md).
The counter is best-effort and does not include requests made by other R
sessions or tools.

## See also

[`ncei_data()`](https://connorb.github.io/preMetabolizer/reference/ncei_data.md)
and
[`ncei_stations()`](https://connorb.github.io/preMetabolizer/reference/ncei_stations.md)
for the parallel NCEI Access Data Service (no token required, different
endpoint structure).

## Examples

``` r
if (FALSE) { # \dontrun{
cdo_datasets()
cdo_datasets("GHCND")
} # }
if (FALSE) { # \dontrun{
cdo_datacategories(datasetid = "GHCND")
} # }
if (FALSE) { # \dontrun{
cdo_datatypes(datasetid = "GHCND", datacategoryid = "TEMP")
cdo_datatypes("TMAX")
} # }
if (FALSE) { # \dontrun{
cdo_locationcategories()
cdo_locationcategories("ST")
} # }
if (FALSE) { # \dontrun{
cdo_locations(locationcategoryid = "ST")
cdo_locations("FIPS:37")
} # }
if (FALSE) { # \dontrun{
cdo_stations(locationid = "FIPS:37", datasetid = "GHCND")
cdo_stations("GHCND:USW00013722")
} # }
if (FALSE) { # \dontrun{
cdo_data(
  datasetid = "GHCND",
  stationid = "GHCND:USW00013722",
  startdate = "2024-01-01",
  enddate   = "2024-01-31",
  datatypeid = c("TMAX", "TMIN")
)
} # }
```
