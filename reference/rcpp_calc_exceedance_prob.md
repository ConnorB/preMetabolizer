# Calculate exceedance probabilities with C++

Calculates exceedance probabilities using the same plotting position
method and return shape as
[`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md),
but delegates ranking to a C++ implementation.

## Usage

``` r
rcpp_calc_exceedance_prob(
  values,
  remove_zeros = FALSE,
  alpha = 0,
  rm.zero = lifecycle::deprecated()
)
```

## Arguments

- values:

  Numeric vector of values, such as stream discharge.

- remove_zeros:

  Logical. If `TRUE`, zero and negative values are excluded from the
  ranking and returned as `NA`. Defaults to `FALSE`.

- alpha:

  Numeric plotting-position constant between 0 and 0.5. Defaults to 0,
  corresponding to the Weibull plotting position. Common alternatives
  include 0.375 (Blom), 0.4 (Cunnane), 0.44 (Gringorten), and 0.5
  (Hazen).

- rm.zero:

  **\[deprecated\]** Use `remove_zeros` instead.

## Value

A numeric vector of exceedance probabilities. If `remove_zeros = TRUE`,
the returned vector has the same length as the input with `NA` at
positions of zero or negative values.

## See also

[`calc_exceedance_prob()`](https://connorb.github.io/preMetabolizer/reference/calc_exceedance_prob.md)

## Examples

``` r
rcpp_calc_exceedance_prob(c(10, 5, 0, 15, 8, NA, 0, 20))
#> [1] 0.3750 0.6250 0.8125 0.2500 0.5000     NA 0.8125 0.1250
```
