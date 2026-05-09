# Calculate histogram bin width

Computes a suggested bin width for a histogram using one of several
classical rules. `NA` values are silently removed before computation.

## Usage

``` r
calc_bin_width(x, method = "auto")
```

## Arguments

- x:

  Numeric vector of observations. Must contain at least two distinct,
  finite, non-`NA` values.

- method:

  Single character string specifying the binning rule. One of:

  `"auto"`

  :   Minimum of Freedman-Diaconis and Sturges (default). Tends to work
      well across a wide range of distributions.

  `"sturges"`

  :   Sturges' rule: \\h = R / (\log_2 n + 1)\\. Assumes approximate
      normality; tends to undersmooth for large \\n\\.

  `"fd"`

  :   Freedman-Diaconis: \\h = 2 \cdot \mathrm{IQR}(x) \cdot n^{-1/3}\\.
      Robust to outliers. Falls back to Sturges when \\\mathrm{IQR}(x) =
      0\\.

  `"sqrt"`

  :   Square-root rule: \\h = R / \sqrt{n}\\. Simple heuristic used by
      some spreadsheet applications.

  `"rice"`

  :   Rice rule: \\h = R / (2 n^{1/3})\\. Similar to Sturges but grows
      more slowly.

  `"scott"`

  :   Scott's rule: \\h = 3.49 \hat\sigma n^{-1/3}\\. Optimal for normal
      data in the sense of minimising mean integrated squared error.

  `"doane"`

  :   Doane's rule. Extends Sturges to account for skewness; better
      suited to non-normal distributions. Requires \\n \ge 3\\.

## Value

A single positive numeric value giving the bin width.

## Details

In all formulae, \\R\\ denotes the range (`diff(range(x))`) and \\n\\
the number of non-`NA` observations.

The `"fd"` fallback to Sturges when `IQR(x) == 0` avoids a zero-width
bin, which can occur with heavily discrete or zero-inflated data.

## Errors

The function aborts with an informative message when:

- `x` is not a numeric vector.

- `method` is not a single character string.

- `x` contains no non-`NA` values, or only one.

- All non-`NA` values in `x` are identical (zero range).

- `method = "doane"` is requested with fewer than 3 observations (the
  skewness standard error is undefined for \\n \< 3\\).

- An unrecognised `method` string is supplied.

## Examples

``` r
# Continuous data — Freedman-Diaconis
calc_bin_width(rnorm(200), "fd")
#> [1] 0.448634

# Skewed data — Doane corrects for skewness
calc_bin_width(rexp(200), "doane")
#> [1] 0.3778829

# Discrete / zero-inflated data — FD falls back to Sturges
calc_bin_width(c(rep(0, 50), rep(1, 50)), "fd")
#> [1] 0.4308869

# NA values are silently dropped
calc_bin_width(c(NA, rnorm(100), NA), "scott")
#> [1] 0.7285563

# Compare all methods on the same data
x <- rnorm(500)
methods <- c("auto", "sturges", "fd", "sqrt", "rice", "scott", "doane")
vapply(methods, \(m) calc_bin_width(x, m), numeric(1))
#>      auto   sturges        fd      sqrt      rice     scott     doane 
#> 0.3280859 0.5811504 0.3280859 0.2590091 0.3648492 0.4261072 0.5560889 

# Error conditions
try(calc_bin_width(letters))               # non-numeric x
#> Error in calc_bin_width(letters) : `x` must be a numeric vector.
#> ✖ Got an object of class <character>.
try(calc_bin_width(c(1, NA, NA)))          # only one non-NA value
#> Error in calc_bin_width(c(1, NA, NA)) : 
#>   `x` contains only one non-"NA" value.
#> ℹ At least two observations are required to compute a bin width.
try(calc_bin_width(rep(5, 50)))            # zero range
#> Error in calc_bin_width(rep(5, 50)) : 
#>   `x` has zero range: all non-"NA" values are identical.
#> ℹ Bin width is undefined when all observations are the same.
try(calc_bin_width(rnorm(200), "badrule")) # unknown method
#> Error in calc_bin_width(rnorm(200), "badrule") : 
#>   Invalid `method` argument.
#> ✖ Method "badrule" is not supported.
#> ℹ Allowed methods: "auto", "sturges", "fd", "sqrt", "rice", "scott", and
#>   "doane"
try(calc_bin_width(c(1, 2), "doane"))      # doane needs n >= 3
#> Error in calc_bin_width(c(1, 2), "doane") : 
#>   Doane's rule requires at least 3 observations.
#> ✖ `x` has only 2 non-"NA" values after removing "NA"s.
#> ℹ Use a different `method` or supply more data.
```
