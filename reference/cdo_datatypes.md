# Query the NCEI CDO data types endpoint (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`cdo_data_types()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
instead.

## Usage

``` r
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
```

## Examples

``` r
# Old:
# cdo_datatypes(datasetid = "GHCND", datacategoryid = "TEMP")
# New:
if (FALSE) { # \dontrun{
cdo_data_types(dataset_id = "GHCND", data_category_id = "TEMP")
} # }
```
