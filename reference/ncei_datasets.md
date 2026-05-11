# List available NCEI datasets

Retrieves metadata about a dataset from the NCEI Support Service API,
including available data types, spatial coverage, and temporal range.

## Usage

``` r
ncei_datasets(dataset)
```

## Arguments

- dataset:

  Character string. The dataset identifier, such as `"daily-summaries"`
  or `"global-hourly"`.

## Value

A list of dataset metadata as returned by the Support Service API. The
structure varies by dataset but typically includes fields such as `id`,
`name`, `dataTypes`, and `extent`.

## Examples

``` r
if (FALSE) { # \dontrun{
ncei_datasets("daily-summaries")
ncei_datasets("global-hourly")
} # }
```
