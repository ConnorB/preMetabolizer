# Calculate the coefficient of variation

Computes the coefficient of variation (CV), a unitless measure of
relative variability. By default, the result is returned as a
percentage.

## Usage

``` r
calc_cv(x, na.rm = TRUE, as_percent = TRUE, robust = FALSE)
```

## Arguments

- x:

  Numeric vector.

- na.rm:

  Logical. If `TRUE`, remove missing values before calculation. Defaults
  to `TRUE`.

- as_percent:

  Logical. If `TRUE`, multiply the CV by 100. Defaults to `TRUE`.

- robust:

  Logical. If `TRUE`, use median and MAD instead of mean and standard
  deviation. Defaults to `FALSE`.

## Value

A single numeric CV value, or `NA_real_` when the input is non-numeric,
empty, all missing after `NA` removal, or centered on zero. Problematic
inputs also produce a warning.

## Examples

``` r
discharge <- c(0.12, 0.18, 0.15, 1.4, 0.09)

calc_cv(discharge)
#> [1] 146.0615
calc_cv(discharge, as_percent = FALSE)
#> [1] 1.460615
calc_cv(discharge, robust = TRUE)
#> [1] 20
```
