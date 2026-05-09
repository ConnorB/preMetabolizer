# Calculate Coefficient of Variation

Computes the coefficient of variation (CV), defined as the ratio of the
standard deviation to the mean. Handles NA values, zero-mean cases, and
provides percentage formatting.

## Usage

``` r
calc_cv(x, na.rm = TRUE, as_percent = TRUE, robust = FALSE)
```

## Arguments

- x:

  A numeric vector for which to compute CV.

- na.rm:

  Logical indicating whether to remove NA values (default: TRUE).

- as_percent:

  Logical indicating whether to return result as percentage (default:
  TRUE).

- robust:

  Logical indicating whether to use median/MAD instead of mean/SD for
  robust CV (default: FALSE).

## Value

The CV as numeric (percentage if `as_percent = TRUE`). Returns NA with
warning for:

- Non-numeric input

- Zero-length input

- All-NA input (when na.rm = TRUE)

- Zero-mean input (for non-robust version)

## Examples

``` r
calc_cv(c(10, 20, 30, 40, 50))  # 47.14045 (percentage)
#> [1] 52.70463
calc_cv(c(10, 20, 30, 40, 50), as_percent = FALSE)  # 0.4714045
#> [1] 0.5270463
calc_cv(c(10, 20, NA, 40, 50))  # NA removed by default
#> [1] 60.85806
calc_cv(c(10, 20, NA, 40, 50), na.rm = FALSE)  # NA
#> [1] NA
calc_cv(numeric(0))  # NA with warning
#> Warning: Empty input vector
#> [1] NA
calc_cv(c(0, 0, 0))  # NA with warning (zero mean)
#> Warning: Mean is zero, CV cannot be calculated
#> [1] NA
calc_cv(c(0, 0, 0), robust = TRUE)  # Computes robust CV
#> Warning: Median is zero, CV cannot be calculated
#> [1] NA
```
