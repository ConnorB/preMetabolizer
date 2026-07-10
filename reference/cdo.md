# Query the NCEI Climate Data Online (CDO) Web Services v2 API

These functions wrap the seven endpoints of NOAA's Climate Data Online
Web Services v2 API (`https://www.ncei.noaa.gov/cdo-web/api/v2`). They
retrieve observations (`cdo_data()`), search the station catalog
(`cdo_stations()`), and browse the metadata catalog (`cdo_datasets()`,
`cdo_data_categories()`, `cdo_data_types()`,
`cdo_location_categories()`, `cdo_locations()`).

## Usage

``` r
cdo_datasets(
  id = NULL,
  datatype_id = NULL,
  location_id = NULL,
  station_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf,
  datatypeid = lifecycle::deprecated(),
  locationid = lifecycle::deprecated(),
  stationid = lifecycle::deprecated(),
  startdate = lifecycle::deprecated(),
  enddate = lifecycle::deprecated(),
  sortfield = lifecycle::deprecated(),
  sortorder = lifecycle::deprecated()
)

cdo_data_categories(
  id = NULL,
  dataset_id = NULL,
  location_id = NULL,
  station_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf
)

cdo_data_types(
  id = NULL,
  dataset_id = NULL,
  location_id = NULL,
  station_id = NULL,
  data_category_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf
)

cdo_location_categories(
  id = NULL,
  dataset_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf
)

cdo_locations(
  id = NULL,
  dataset_id = NULL,
  location_category_id = NULL,
  data_category_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf,
  datasetid = lifecycle::deprecated(),
  locationcategoryid = lifecycle::deprecated(),
  datacategoryid = lifecycle::deprecated(),
  startdate = lifecycle::deprecated(),
  enddate = lifecycle::deprecated(),
  sortfield = lifecycle::deprecated(),
  sortorder = lifecycle::deprecated()
)

cdo_stations(
  id = NULL,
  dataset_id = NULL,
  location_id = NULL,
  data_category_id = NULL,
  datatype_id = NULL,
  extent = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf,
  datasetid = lifecycle::deprecated(),
  locationid = lifecycle::deprecated(),
  datacategoryid = lifecycle::deprecated(),
  datatypeid = lifecycle::deprecated(),
  startdate = lifecycle::deprecated(),
  enddate = lifecycle::deprecated(),
  sortfield = lifecycle::deprecated(),
  sortorder = lifecycle::deprecated()
)

cdo_data(
  dataset_id = NULL,
  start_date = NULL,
  end_date = NULL,
  datatype_id = NULL,
  location_id = NULL,
  station_id = NULL,
  units = "metric",
  sort_field = NULL,
  sort_order = NULL,
  include_metadata = FALSE,
  max_results = Inf,
  datasetid = lifecycle::deprecated(),
  startdate = lifecycle::deprecated(),
  enddate = lifecycle::deprecated(),
  datatypeid = lifecycle::deprecated(),
  locationid = lifecycle::deprecated(),
  stationid = lifecycle::deprecated(),
  sortfield = lifecycle::deprecated(),
  sortorder = lifecycle::deprecated(),
  includemetadata = lifecycle::deprecated()
)
```

## Arguments

- id:

  Optional character string. When supplied, fetches a single resource by
  its identifier (e.g., `cdo_datasets("GHCND")`) and returns a named
  list. When `NULL` (default) the list endpoint is queried and a tibble
  is returned.

- start_date, end_date:

  Optional date filters as `Date` objects or `"YYYY-MM-DD"` strings. For
  `cdo_data()` both are required.

- sort_field, sort_order:

  Optional sort controls. `sort_field` is one of `"id"`, `"name"`,
  `"mindate"`, `"maxdate"`, `"datacoverage"`; `sort_order` is `"asc"`
  (default) or `"desc"`.

- max_results:

  Maximum number of rows to return. Defaults to `Inf` (all matching
  results).

- dataset_id, datatype_id, location_id, station_id, data_category_id,
  location_category_id:

  Optional character vectors filtering the result set. Multiple values
  are sent as repeated query parameters.

- datasetid, datatypeid, locationid, stationid, datacategoryid,
  locationcategoryid, startdate, enddate, sortfield, sortorder,
  includemetadata:

  **\[deprecated\]** Use `dataset_id`, `datatype_id`, `location_id`,
  `station_id`, `data_category_id`, `location_category_id`,
  `start_date`, `end_date`, `sort_field`, `sort_order`, and
  `include_metadata` instead.

- extent:

  For `cdo_stations()` only. Numeric vector of length 4 giving a
  bounding box as `c(min_lat, min_lon, max_lat, max_lon)`.

- units:

  For `cdo_data()` only. `"metric"` (default) or `"standard"`.

- include_metadata:

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
cdo_data_categories(dataset_id = "GHCND")
} # }
if (FALSE) { # \dontrun{
cdo_data_types(dataset_id = "GHCND", data_category_id = "TEMP")
cdo_data_types("TMAX")
} # }
if (FALSE) { # \dontrun{
cdo_location_categories()
cdo_location_categories("ST")
} # }
if (FALSE) { # \dontrun{
cdo_locations(location_category_id = "ST")
cdo_locations("FIPS:37")
} # }
if (FALSE) { # \dontrun{
cdo_stations(location_id = "FIPS:37", dataset_id = "GHCND")
cdo_stations("GHCND:USW00013722")
} # }
if (FALSE) { # \dontrun{
cdo_data(
  dataset_id = "GHCND",
  station_id = "GHCND:USW00013722",
  start_date = "2024-01-01",
  end_date   = "2024-01-31",
  datatype_id = c("TMAX", "TMIN")
)
} # }
```
