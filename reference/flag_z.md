# Flag outliers with robust Z-scores

Identifies potential outliers in a numeric vector based on a moving
window robust Z-score approach. The robust Z-score is computed using a
biweight scale estimate centered on the median.

## Usage

``` r
flag_z(x, width = 5, threshold = 3, return_z = FALSE)
```

## Arguments

- x:

  Numeric vector to check for outliers.

- width:

  Odd integer giving the moving-window width. Defaults to `5`.

- threshold:

  Numeric threshold for the absolute Z-score above which a value is
  flagged. Defaults to `3`.

- return_z:

  Logical. If `TRUE`, return both Z-scores and flags. If `FALSE`, return
  only flags. Defaults to `FALSE`.

## Value

If `return_z = TRUE`, a list with:

- z:

  A numeric vector of robust Z-scores (with `NA` where not computable).

- flag:

  A character vector of the same length as `x`, with `"Z"` where an
  outlier is detected and `NA` otherwise.

If `return_z = FALSE`, only the `flag` vector is returned.

## Details

For each value in `x`, a window of length `width` centered on that value
is extracted. The function:

- Computes the median of the window.

- Calculates residuals from the median and MAD-based scale.

- Estimates a robust scale using the Tukey biweight midvariance with
  tuning constant `c = 9` (Mosteller & Tukey 1977; Lax 1985): \$\$s^2 =
  n \cdot \frac{\sum r_i^2 (1 - u_i^2)^4}{\left\[\sum (1 - u_i^2)(1 - 5
  u_i^2)\right\]^2}\$\$ where \\u_i = r_i / (c \cdot \mathrm{MAD})\\ and
  the sums run over \\\|u_i\| \< 1\\.

- Computes a Z-score as \\(x_i - \text{median}) / s\\.

If the absolute value of the Z-score exceeds `threshold`, the value is
flagged with `"Z"`.

NA values are ignored in the window statistics but retained in output
positions.

## References

Mosteller, F. and Tukey, J. W. (1977). *Data Analysis and Regression*.
Addison-Wesley.

Lax, D. A. (1985). Robust estimators of scale: Finite-sample performance
in long-tailed symmetric distributions. *Journal of the American
Statistical Association*, 80(391), 736–741.

## See also

[`stats::mad()`](https://rdrr.io/r/stats/mad.html),
[`stats::median()`](https://rdrr.io/r/stats/median.html)

## Examples

``` r
x <- c(1, 2, 1.5, 1.2, 100, 1.1, 1.3, 1.4)
flag_z(x)
#> [1] NA  "Z" NA  NA  "Z" NA  NA  NA 
flag_z(x, return_z = TRUE)
#> $z
#> [1]  -1.1941900   3.7088929   0.0000000  -0.7372456 577.2180895  -1.4041012
#> [7]  -1.1403570   0.7428716
#> 
#> $flag
#> [1] NA  "Z" NA  NA  "Z" NA  NA  NA 
#> 
```
