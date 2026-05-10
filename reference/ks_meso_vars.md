# Fetch Kansas Mesonet variable metadata

Retrieves variable metadata from the Kansas Mesonet REST variables page
and returns a tidy table of available variables, including their CSV
headings, original variable names, cleaned variable names, units, and
descriptions.

## Usage

``` r
ks_meso_vars()
```

## Value

A tibble with one row per Kansas Mesonet variable and the columns:

- csv_heading:

  The variable heading used in Mesonet CSV output.

- var:

  The original variable name from the Mesonet metadata.

- tidy_name:

  A cleaned, snake_case version of `var`.

- units:

  The measurement units for the variable.

- desc:

  A description of the variable.

## Details

**\[experimental\]**

The function downloads the Kansas Mesonet variables page, extracts the
embedded JavaScript metadata array, parses variable fields, and
standardizes variable names into `tidy_name`. Kansas Mesonet data are
preliminary and subject to revision. Cite the Kansas Mesonet when
sharing, publishing, or otherwise disseminating data accessed with this
function. A suggested citation format is: Kansas Mesonet, year: webpage
title. Accessed date, webpage URL. Review the Kansas Mesonet data usage
policy before automated use; automated page scraping or data ingesting
without written consent is not permitted.

## References

Kansas Mesonet data usage policy:
<https://mesonet.k-state.edu/about/usage/>

## Examples

``` r
if (FALSE) { # \dontrun{
vars <- ks_meso_vars()
vars
} # }
```
