# Query the NCEI CDO data categories endpoint (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`cdo_data_categories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
instead.

## Usage

``` r
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
```

## Examples

``` r
# Old:
# cdo_datacategories(datasetid = "GHCND")
# New:
if (FALSE) { # \dontrun{
cdo_data_categories(dataset_id = "GHCND")
} # }
```
