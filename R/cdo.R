#' Query the NCEI Climate Data Online (CDO) Web Services v2 API
#'
#' These functions wrap the seven endpoints of NOAA's Climate Data Online
#' Web Services v2 API
#' (`https://www.ncei.noaa.gov/cdo-web/api/v2`). They retrieve
#' observations (`cdo_data()`), search the station catalog
#' (`cdo_stations()`), and browse the metadata catalog
#' (`cdo_datasets()`, `cdo_datacategories()`, `cdo_datatypes()`,
#' `cdo_locationcategories()`, `cdo_locations()`).
#'
#' @section Authentication:
#' All CDO endpoints require a free API token. Request one at
#' \url{https://www.ncdc.noaa.gov/cdo-web/token} and store it in the
#' `API_NCEI_CDO` environment variable, for example by adding
#' `API_NCEI_CDO=...` to your `~/.Renviron`.
#'
#' @section Pagination:
#' List endpoints paginate at 1000 results per page. By default each
#' function transparently walks all pages and returns a single tibble.
#' Use `max_results` to cap the total number of rows returned.
#'
#' @section Rate limits:
#' Each token is limited to 5 requests per second and 10,000 requests
#' per day. The 5/s cap is enforced client-side via
#' [httr2::req_throttle()] (requests block briefly rather than failing).
#' The package also tracks a session-local counter; once 10,000
#' successful requests have been made in the current R session the next
#' call aborts with an error directing you to
#' [cdo_reset_request_count()]. The counter is best-effort and does not
#' include requests made by other R sessions or tools.
#'
#' @param id Optional character string. When supplied, fetches a single
#'   resource by its identifier (e.g., `cdo_datasets("GHCND")`) and
#'   returns a named list. When `NULL` (default) the list endpoint is
#'   queried and a tibble is returned.
#' @param datasetid,datatypeid,locationid,stationid,datacategoryid,locationcategoryid
#'   Optional character vectors filtering the result set. Multiple
#'   values are sent as repeated query parameters.
#' @param startdate,enddate Optional date filters as `Date` objects or
#'   `"YYYY-MM-DD"` strings. For `cdo_data()` both are required.
#' @param sortfield,sortorder Optional sort controls. `sortfield` is one
#'   of `"id"`, `"name"`, `"mindate"`, `"maxdate"`, `"datacoverage"`;
#'   `sortorder` is `"asc"` (default) or `"desc"`.
#' @param max_results Maximum number of rows to return. Defaults to
#'   `Inf` (all matching results).
#' @param extent For `cdo_stations()` only. Numeric vector of length 4
#'   giving a bounding box as `c(min_lat, min_lon, max_lat, max_lon)`.
#' @param units For `cdo_data()` only. `"metric"` (default) or
#'   `"standard"`.
#' @param includemetadata For `cdo_data()` only. Logical; when `FALSE`
#'   (default) the API skips computing the result-set count, which can
#'   noticeably speed up large requests.
#'
#' @return A [tibble][tibble::tibble-package] for list queries, or a
#'   named list for single-resource (`id`-based) queries. Date-like
#'   columns (`mindate`, `maxdate`, `date`) are parsed to `Date` /
#'   `POSIXct` and numeric columns (`latitude`, `longitude`,
#'   `elevation`, `value`, `datacoverage`) are coerced to numeric.
#'
#' @seealso [ncei_data()] and [ncei_stations()] for the parallel NCEI
#'   Access Data Service (no token required, different endpoint
#'   structure).
#'
#' @name cdo
NULL

cdo_apply_filters <- function(req, ...) {
  filters <- list(...)
  filters <- filters[!vapply(filters, is.null, logical(1))]
  if (length(filters) == 0) {
    return(req)
  }
  rlang::inject(httr2::req_url_query(
    req,
    !!!filters,
    .multi = "explode"
  ))
}

cdo_check_sort <- function(
  sortfield,
  sortorder,
  call = rlang::caller_env()
) {
  if (!is.null(sortfield)) {
    sortfield <- rlang::arg_match(
      sortfield,
      c("id", "name", "mindate", "maxdate", "datacoverage"),
      error_call = call
    )
  }
  if (!is.null(sortorder)) {
    sortorder <- rlang::arg_match(
      sortorder,
      c("asc", "desc"),
      error_call = call
    )
  }
  list(sortfield = sortfield, sortorder = sortorder)
}

cdo_normalise_dates <- function(startdate, enddate) {
  if (!is.null(startdate)) {
    startdate <- ncei_check_date(startdate)
  }
  if (!is.null(enddate)) {
    enddate <- ncei_check_date(enddate)
  }
  list(startdate = startdate, enddate = enddate)
}

cdo_get_one <- function(path, error_message) {
  req <- cdo_request(path)
  cdo_perform(req, error_message)
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_datasets()
#' cdo_datasets("GHCND")
#' }
cdo_datasets <- function(
  id = NULL,
  datatypeid = NULL,
  locationid = NULL,
  stationid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datasets/", id),
      "Failed to retrieve dataset from the NCEI CDO API."
    ))
  }
  check_character(datatypeid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(stationid, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datasets") |>
    cdo_apply_filters(
      datatypeid = datatypeid,
      locationid = locationid,
      stationid = stationid,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve datasets from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_datacategories(datasetid = "GHCND")
#' }
cdo_datacategories <- function(
  id = NULL,
  datasetid = NULL,
  locationid = NULL,
  stationid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datacategories/", id),
      "Failed to retrieve data category from the NCEI CDO API."
    ))
  }
  check_character(datasetid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(stationid, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datacategories") |>
    cdo_apply_filters(
      datasetid = datasetid,
      locationid = locationid,
      stationid = stationid,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data categories from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_datatypes(datasetid = "GHCND", datacategoryid = "TEMP")
#' cdo_datatypes("TMAX")
#' }
cdo_datatypes <- function(
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
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datatypes/", id),
      "Failed to retrieve data type from the NCEI CDO API."
    ))
  }
  check_character(datasetid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(stationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(datacategoryid, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datatypes") |>
    cdo_apply_filters(
      datasetid = datasetid,
      locationid = locationid,
      stationid = stationid,
      datacategoryid = datacategoryid,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data types from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_locationcategories()
#' cdo_locationcategories("ST")
#' }
cdo_locationcategories <- function(
  id = NULL,
  datasetid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/locationcategories/", id),
      "Failed to retrieve location category from the NCEI CDO API."
    ))
  }
  check_character(datasetid, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/locationcategories") |>
    cdo_apply_filters(
      datasetid = datasetid,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve location categories from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_locations(locationcategoryid = "ST")
#' cdo_locations("FIPS:37")
#' }
cdo_locations <- function(
  id = NULL,
  datasetid = NULL,
  locationcategoryid = NULL,
  datacategoryid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/locations/", id),
      "Failed to retrieve location from the NCEI CDO API."
    ))
  }
  check_character(datasetid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationcategoryid, allow_null = TRUE, allow_empty = FALSE)
  check_character(datacategoryid, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/locations") |>
    cdo_apply_filters(
      datasetid = datasetid,
      locationcategoryid = locationcategoryid,
      datacategoryid = datacategoryid,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve locations from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_stations(locationid = "FIPS:37", datasetid = "GHCND")
#' cdo_stations("GHCND:USW00013722")
#' }
cdo_stations <- function(
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
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/stations/", id),
      "Failed to retrieve station from the NCEI CDO API."
    ))
  }
  check_character(datasetid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(datacategoryid, allow_null = TRUE, allow_empty = FALSE)
  check_character(datatypeid, allow_null = TRUE, allow_empty = FALSE)
  extent_str <- NULL
  if (!is.null(extent)) {
    check_numeric(extent, allow_na = FALSE, allow_infinite = FALSE)
    if (length(extent) != 4) {
      cli::cli_abort(
        "{.arg extent} must be a length-4 numeric vector: c(min_lat, min_lon, max_lat, max_lon)."
      )
    }
    if (abs(extent[1]) > 90 || abs(extent[3]) > 90) {
      cli::cli_abort("{.arg extent} latitudes must be between -90 and 90.")
    }
    if (abs(extent[2]) > 180 || abs(extent[4]) > 180) {
      cli::cli_abort("{.arg extent} longitudes must be between -180 and 180.")
    }
    if (extent[3] < extent[1] || extent[4] < extent[2]) {
      cli::cli_abort(
        "{.arg extent}: max_lat/max_lon must be >= min_lat/min_lon."
      )
    }
    extent_str <- sprintf(
      "%.6f,%.6f,%.6f,%.6f",
      extent[1],
      extent[2],
      extent[3],
      extent[4]
    )
  }
  dates <- cdo_normalise_dates(startdate, enddate)
  sort <- cdo_check_sort(sortfield, sortorder)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/stations") |>
    cdo_apply_filters(
      datasetid = datasetid,
      locationid = locationid,
      datacategoryid = datacategoryid,
      datatypeid = datatypeid,
      extent = extent_str,
      startdate = dates$startdate,
      enddate = dates$enddate,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve stations from the NCEI CDO API."
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_data(
#'   datasetid = "GHCND",
#'   stationid = "GHCND:USW00013722",
#'   startdate = "2024-01-01",
#'   enddate   = "2024-01-31",
#'   datatypeid = c("TMAX", "TMIN")
#' )
#' }
cdo_data <- function(
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
) {
  check_string(datasetid, allow_empty = FALSE)
  startdate <- ncei_check_date(startdate)
  enddate <- ncei_check_date(enddate)
  check_character(datatypeid, allow_null = TRUE, allow_empty = FALSE)
  check_character(locationid, allow_null = TRUE, allow_empty = FALSE)
  check_character(stationid, allow_null = TRUE, allow_empty = FALSE)
  units <- rlang::arg_match(units, c("metric", "standard"))
  sort <- cdo_check_sort(sortfield, sortorder)
  check_bool(includemetadata)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/data") |>
    cdo_apply_filters(
      datasetid = datasetid,
      startdate = startdate,
      enddate = enddate,
      datatypeid = datatypeid,
      locationid = locationid,
      stationid = stationid,
      units = units,
      sortfield = sort$sortfield,
      sortorder = sort$sortorder,
      includemetadata = if (includemetadata) "true" else "false"
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data from the NCEI CDO API."
  )
}
