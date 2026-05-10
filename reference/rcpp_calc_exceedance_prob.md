# Calculate flow exceedance probabilities with C++

Calculates exceedance probabilities using the same Weibull plotting
position method and return shape as
[`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md),
but delegates ranking to a C++ implementation.

## Usage

``` r
rcpp_calc_exceedance_prob(flow, rm.zero = FALSE)
```

## Arguments

- flow:

  Numeric vector of flow or discharge values.

- rm.zero:

  Logical. If `TRUE`, zero and negative values are omitted from the
  ranking and returned as `NA`. Defaults to `FALSE`.

## Value

A numeric vector of exceedance probabilities. If `rm.zero = TRUE`, the
returned vector has the same length as the input with `NA` at positions
of zero or negative values.

## See also

[`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md)

## Examples

``` r
rcpp_calc_exceedance_prob(c(10, 5, 0, 15, 8, NA, 0, 20))
#> [1] 0.3750 0.6250 0.8125 0.2500 0.5000     NA 0.8125 0.1250
```
