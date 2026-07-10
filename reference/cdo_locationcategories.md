# Query the NCEI CDO location categories endpoint (deprecated)

**\[deprecated\]**

This function is deprecated. Please use
[`cdo_location_categories()`](https://connorb.github.io/preMetabolizer/reference/cdo.md)
instead.

## Usage

``` r
cdo_locationcategories(
  id = NULL,
  datasetid = NULL,
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
# cdo_locationcategories("ST")
# New:
if (FALSE) { # \dontrun{
cdo_location_categories("ST")
} # }
```
