# Flag Outliers Using Robust Z-Scores

Identifies potential outliers in a numeric vector based on a moving
window robust Z-score approach. The robust Z-score is computed using a
biweight scale estimate centered on the median.

## Usage

``` r
flag_z(x, width = 5, threshold = 3, return_z = FALSE)
```

## Arguments

- x:

  A numeric vector to be checked for outliers.

- width:

  An odd integer specifying the width of the moving window (default: 5).

- threshold:

  A numeric threshold for the absolute Z-score above which a value is
  flagged (default: 3.0).

- return_z:

  Logical; if `TRUE`, returns both Z-scores and flags. If `FALSE`,
  returns only flags (default: `FALSE`).

## Value

If `return_z = TRUE`, a list with:

- z:

  A numeric vector of robust Z-scores (with `NA` where not computable).

- flag:

  A character vector of same length as `x`, with `"Z"` where an outlier
  is detected, and `NA` otherwise.

If `return_z = FALSE`, only the `flag` vector is returned.

## Details

For each value in `x`, a window of length `width` centered on that value
is extracted. The function:

- Computes the median of the window.

- Calculates residuals from the median.

- Estimates a robust scale using Tukey’s biweight estimator based on
  MAD.

- Computes a Z-score as \\(x_i - \text{median}) / \text{scale}\\.

If the absolute value of the Z-score exceeds `threshold`, the value is
flagged with `"Z"`.

NA values are ignored in the window statistics but retained in output
positions.

## See also

[`stats::mad()`](https://rdrr.io/r/stats/mad.html),
[`stats::median()`](https://rdrr.io/r/stats/median.html)

## Examples

``` r
x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
flag_z(x)
#> [1] NA  NA  NA  NA  "Z" NA  NA  NA 
flag_z(x, return_z = TRUE)
#> $z
#> [1]  -0.8538437   2.8070970   0.0000000  -0.5964796 459.3115264  -1.2804234
#> [7]  -1.0015789   0.6289591
#> 
#> $flag
#> [1] NA  NA  NA  NA  "Z" NA  NA  NA 
#> 
```
