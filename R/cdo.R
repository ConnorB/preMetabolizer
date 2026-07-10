#' Query the NCEI Climate Data Online (CDO) Web Services v2 API
#'
#' These functions wrap the seven endpoints of NOAA's Climate Data Online
#' Web Services v2 API
#' (`https://www.ncei.noaa.gov/cdo-web/api/v2`). They retrieve
#' observations (`cdo_data()`), search the station catalog
#' (`cdo_stations()`), and browse the metadata catalog
#' (`cdo_datasets()`, `cdo_data_categories()`, `cdo_data_types()`,
#' `cdo_location_categories()`, `cdo_locations()`).
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
#' @param dataset_id,datatype_id,location_id,station_id,data_category_id,location_category_id
#'   Optional character vectors filtering the result set. Multiple
#'   values are sent as repeated query parameters.
#' @param start_date,end_date Optional date filters as `Date` objects or
#'   `"YYYY-MM-DD"` strings. For `cdo_data()` both are required.
#' @param sort_field,sort_order Optional sort controls. `sort_field` is one
#'   of `"id"`, `"name"`, `"mindate"`, `"maxdate"`, `"datacoverage"`;
#'   `sort_order` is `"asc"` (default) or `"desc"`.
#' @param max_results Maximum number of rows to return. Defaults to
#'   `Inf` (all matching results).
#' @param extent For `cdo_stations()` only. Numeric vector of length 4
#'   giving a bounding box as `c(min_lat, min_lon, max_lat, max_lon)`.
#' @param units For `cdo_data()` only. `"metric"` (default) or
#'   `"standard"`.
#' @param include_metadata For `cdo_data()` only. Logical; when `FALSE`
#'   (default) the API skips computing the result-set count, which can
#'   noticeably speed up large requests.
#' @param datasetid,datatypeid,locationid,stationid,datacategoryid,locationcategoryid,startdate,enddate,sortfield,sortorder,includemetadata
#'   `r lifecycle::badge("deprecated")` Use `dataset_id`, `datatype_id`,
#'   `location_id`, `station_id`, `data_category_id`,
#'   `location_category_id`, `start_date`, `end_date`, `sort_field`,
#'   `sort_order`, and `include_metadata` instead.
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
  sort_field,
  sort_order,
  call = rlang::caller_env()
) {
  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
      c("id", "name", "mindate", "maxdate", "datacoverage"),
      error_call = call
    )
  }
  if (!is.null(sort_order)) {
    sort_order <- rlang::arg_match(
      sort_order,
      c("asc", "desc"),
      error_call = call
    )
  }
  list(sort_field = sort_field, sort_order = sort_order)
}

cdo_normalise_dates <- function(start_date, end_date) {
  if (!is.null(start_date)) {
    start_date <- ncei_check_date(start_date)
  }
  if (!is.null(end_date)) {
    end_date <- ncei_check_date(end_date)
  }
  list(start_date = start_date, end_date = end_date)
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
) {
  datatype_id <- cdo_deprecate_arg(
    datatype_id,
    datatypeid,
    "datatype_id",
    "datatypeid",
    "cdo_datasets"
  )
  location_id <- cdo_deprecate_arg(
    location_id,
    locationid,
    "location_id",
    "locationid",
    "cdo_datasets"
  )
  station_id <- cdo_deprecate_arg(
    station_id,
    stationid,
    "station_id",
    "stationid",
    "cdo_datasets"
  )
  start_date <- cdo_deprecate_arg(
    start_date,
    startdate,
    "start_date",
    "startdate",
    "cdo_datasets"
  )
  end_date <- cdo_deprecate_arg(
    end_date,
    enddate,
    "end_date",
    "enddate",
    "cdo_datasets"
  )
  sort_field <- cdo_deprecate_arg(
    sort_field,
    sortfield,
    "sort_field",
    "sortfield",
    "cdo_datasets"
  )
  sort_order <- cdo_deprecate_arg(
    sort_order,
    sortorder,
    "sort_order",
    "sortorder",
    "cdo_datasets"
  )

  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datasets/", id),
      "Failed to retrieve dataset from the NCEI CDO API."
    ))
  }
  check_character(datatype_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(station_id, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datasets") |>
    cdo_apply_filters(
      datatypeid = datatype_id,
      locationid = location_id,
      stationid = station_id,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
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
#' cdo_data_categories(dataset_id = "GHCND")
#' }
cdo_data_categories <- function(
  id = NULL,
  dataset_id = NULL,
  location_id = NULL,
  station_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datacategories/", id),
      "Failed to retrieve data category from the NCEI CDO API."
    ))
  }
  check_character(dataset_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(station_id, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datacategories") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      locationid = location_id,
      stationid = station_id,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data categories from the NCEI CDO API."
  )
}

#' Query the NCEI CDO data categories endpoint (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [cdo_data_categories()] instead.
#'
#' @examples
#' # Old:
#' # cdo_datacategories(datasetid = "GHCND")
#' # New:
#' \dontrun{
#' cdo_data_categories(dataset_id = "GHCND")
#' }
#'
#' @keywords internal
#' @export
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
  lifecycle::deprecate_soft(
    "0.0.0.9000",
    "cdo_datacategories()",
    "cdo_data_categories()"
  )
  cdo_data_categories(
    id = id,
    dataset_id = datasetid,
    location_id = locationid,
    station_id = stationid,
    start_date = startdate,
    end_date = enddate,
    sort_field = sortfield,
    sort_order = sortorder,
    max_results = max_results
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_data_types(dataset_id = "GHCND", data_category_id = "TEMP")
#' cdo_data_types("TMAX")
#' }
cdo_data_types <- function(
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
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/datatypes/", id),
      "Failed to retrieve data type from the NCEI CDO API."
    ))
  }
  check_character(dataset_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(station_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(data_category_id, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/datatypes") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      locationid = location_id,
      stationid = station_id,
      datacategoryid = data_category_id,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data types from the NCEI CDO API."
  )
}

#' Query the NCEI CDO data types endpoint (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [cdo_data_types()] instead.
#'
#' @examples
#' # Old:
#' # cdo_datatypes(datasetid = "GHCND", datacategoryid = "TEMP")
#' # New:
#' \dontrun{
#' cdo_data_types(dataset_id = "GHCND", data_category_id = "TEMP")
#' }
#'
#' @keywords internal
#' @export
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
  lifecycle::deprecate_soft(
    "0.0.0.9000",
    "cdo_datatypes()",
    "cdo_data_types()"
  )
  cdo_data_types(
    id = id,
    dataset_id = datasetid,
    location_id = locationid,
    station_id = stationid,
    data_category_id = datacategoryid,
    start_date = startdate,
    end_date = enddate,
    sort_field = sortfield,
    sort_order = sortorder,
    max_results = max_results
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_location_categories()
#' cdo_location_categories("ST")
#' }
cdo_location_categories <- function(
  id = NULL,
  dataset_id = NULL,
  start_date = NULL,
  end_date = NULL,
  sort_field = NULL,
  sort_order = NULL,
  max_results = Inf
) {
  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/locationcategories/", id),
      "Failed to retrieve location category from the NCEI CDO API."
    ))
  }
  check_character(dataset_id, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/locationcategories") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve location categories from the NCEI CDO API."
  )
}

#' Query the NCEI CDO location categories endpoint (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [cdo_location_categories()]
#' instead.
#'
#' @examples
#' # Old:
#' # cdo_locationcategories("ST")
#' # New:
#' \dontrun{
#' cdo_location_categories("ST")
#' }
#'
#' @keywords internal
#' @export
cdo_locationcategories <- function(
  id = NULL,
  datasetid = NULL,
  startdate = NULL,
  enddate = NULL,
  sortfield = NULL,
  sortorder = NULL,
  max_results = Inf
) {
  lifecycle::deprecate_soft(
    "0.0.0.9000",
    "cdo_locationcategories()",
    "cdo_location_categories()"
  )
  cdo_location_categories(
    id = id,
    dataset_id = datasetid,
    start_date = startdate,
    end_date = enddate,
    sort_field = sortfield,
    sort_order = sortorder,
    max_results = max_results
  )
}

#' @rdname cdo
#' @export
#' @examples
#' \dontrun{
#' cdo_locations(location_category_id = "ST")
#' cdo_locations("FIPS:37")
#' }
cdo_locations <- function(
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
) {
  dataset_id <- cdo_deprecate_arg(
    dataset_id,
    datasetid,
    "dataset_id",
    "datasetid",
    "cdo_locations"
  )
  location_category_id <- cdo_deprecate_arg(
    location_category_id,
    locationcategoryid,
    "location_category_id",
    "locationcategoryid",
    "cdo_locations"
  )
  data_category_id <- cdo_deprecate_arg(
    data_category_id,
    datacategoryid,
    "data_category_id",
    "datacategoryid",
    "cdo_locations"
  )
  start_date <- cdo_deprecate_arg(
    start_date,
    startdate,
    "start_date",
    "startdate",
    "cdo_locations"
  )
  end_date <- cdo_deprecate_arg(
    end_date,
    enddate,
    "end_date",
    "enddate",
    "cdo_locations"
  )
  sort_field <- cdo_deprecate_arg(
    sort_field,
    sortfield,
    "sort_field",
    "sortfield",
    "cdo_locations"
  )
  sort_order <- cdo_deprecate_arg(
    sort_order,
    sortorder,
    "sort_order",
    "sortorder",
    "cdo_locations"
  )

  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/locations/", id),
      "Failed to retrieve location from the NCEI CDO API."
    ))
  }
  check_character(dataset_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_category_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(data_category_id, allow_null = TRUE, allow_empty = FALSE)
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/locations") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      locationcategoryid = location_category_id,
      datacategoryid = data_category_id,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
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
#' cdo_stations(location_id = "FIPS:37", dataset_id = "GHCND")
#' cdo_stations("GHCND:USW00013722")
#' }
cdo_stations <- function(
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
) {
  dataset_id <- cdo_deprecate_arg(
    dataset_id,
    datasetid,
    "dataset_id",
    "datasetid",
    "cdo_stations"
  )
  location_id <- cdo_deprecate_arg(
    location_id,
    locationid,
    "location_id",
    "locationid",
    "cdo_stations"
  )
  data_category_id <- cdo_deprecate_arg(
    data_category_id,
    datacategoryid,
    "data_category_id",
    "datacategoryid",
    "cdo_stations"
  )
  datatype_id <- cdo_deprecate_arg(
    datatype_id,
    datatypeid,
    "datatype_id",
    "datatypeid",
    "cdo_stations"
  )
  start_date <- cdo_deprecate_arg(
    start_date,
    startdate,
    "start_date",
    "startdate",
    "cdo_stations"
  )
  end_date <- cdo_deprecate_arg(
    end_date,
    enddate,
    "end_date",
    "enddate",
    "cdo_stations"
  )
  sort_field <- cdo_deprecate_arg(
    sort_field,
    sortfield,
    "sort_field",
    "sortfield",
    "cdo_stations"
  )
  sort_order <- cdo_deprecate_arg(
    sort_order,
    sortorder,
    "sort_order",
    "sortorder",
    "cdo_stations"
  )

  check_string(id, allow_null = TRUE, allow_empty = FALSE)
  if (!is.null(id)) {
    return(cdo_get_one(
      paste0("/stations/", id),
      "Failed to retrieve station from the NCEI CDO API."
    ))
  }
  check_character(dataset_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(data_category_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(datatype_id, allow_null = TRUE, allow_empty = FALSE)
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
  dates <- cdo_normalise_dates(start_date, end_date)
  sort <- cdo_check_sort(sort_field, sort_order)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/stations") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      locationid = location_id,
      datacategoryid = data_category_id,
      datatypeid = datatype_id,
      extent = extent_str,
      startdate = dates$start_date,
      enddate = dates$end_date,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order
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
#'   dataset_id = "GHCND",
#'   station_id = "GHCND:USW00013722",
#'   start_date = "2024-01-01",
#'   end_date   = "2024-01-31",
#'   datatype_id = c("TMAX", "TMIN")
#' )
#' }
cdo_data <- function(
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
) {
  dataset_id <- cdo_deprecate_arg(
    dataset_id,
    datasetid,
    "dataset_id",
    "datasetid",
    "cdo_data"
  )
  start_date <- cdo_deprecate_arg(
    start_date,
    startdate,
    "start_date",
    "startdate",
    "cdo_data"
  )
  end_date <- cdo_deprecate_arg(
    end_date,
    enddate,
    "end_date",
    "enddate",
    "cdo_data"
  )
  datatype_id <- cdo_deprecate_arg(
    datatype_id,
    datatypeid,
    "datatype_id",
    "datatypeid",
    "cdo_data"
  )
  location_id <- cdo_deprecate_arg(
    location_id,
    locationid,
    "location_id",
    "locationid",
    "cdo_data"
  )
  station_id <- cdo_deprecate_arg(
    station_id,
    stationid,
    "station_id",
    "stationid",
    "cdo_data"
  )
  sort_field <- cdo_deprecate_arg(
    sort_field,
    sortfield,
    "sort_field",
    "sortfield",
    "cdo_data"
  )
  sort_order <- cdo_deprecate_arg(
    sort_order,
    sortorder,
    "sort_order",
    "sortorder",
    "cdo_data"
  )
  include_metadata <- cdo_deprecate_arg(
    include_metadata,
    includemetadata,
    "include_metadata",
    "includemetadata",
    "cdo_data"
  )

  check_string(dataset_id, allow_empty = FALSE)
  start_date <- ncei_check_date(start_date)
  end_date <- ncei_check_date(end_date)
  check_character(datatype_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(location_id, allow_null = TRUE, allow_empty = FALSE)
  check_character(station_id, allow_null = TRUE, allow_empty = FALSE)
  units <- rlang::arg_match(units, c("metric", "standard"))
  sort <- cdo_check_sort(sort_field, sort_order)
  check_bool(include_metadata)
  max_results <- cdo_check_max_results(max_results)

  req <- cdo_request("/data") |>
    cdo_apply_filters(
      datasetid = dataset_id,
      startdate = start_date,
      enddate = end_date,
      datatypeid = datatype_id,
      locationid = location_id,
      stationid = station_id,
      units = units,
      sortfield = sort$sort_field,
      sortorder = sort$sort_order,
      includemetadata = if (include_metadata) "true" else "false"
    )

  cdo_paginate(
    req,
    max_results = max_results,
    error_message = "Failed to retrieve data from the NCEI CDO API."
  )
}
