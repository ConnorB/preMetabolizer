# Calculate exceedance probabilities

Calculates exceedance probabilities for a numeric vector, such as stream
discharge, using a plotting-position formula. Larger values are assigned
lower exceedance probabilities.

## Usage

``` r
calc_exceedance_prob(
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

A numeric vector of exceedance probabilities with the same length and
order as `values`. Missing and infinite input values are returned as
`NA`.

## Details

Exceedance probabilities are calculated using the general
plotting-position formula: \$\$P = \frac{\mathrm{rank} - \alpha}{n + 1 -
2\alpha}\$\$ where rank is the descending rank of each value, n is the
number of ranked observations, and alpha is the plotting-position
constant.

Tied values are assigned the average of the ranks they would otherwise
occupy, so identical values receive identical exceedance probabilities.

## References

Cunnane, C. (1978). Unbiased plotting positions — A review. *Journal of
Hydrology*, 37(3–4), 205–222.
[doi:10.1016/0022-1694(78)90017-3](https://doi.org/10.1016/0022-1694%2878%2990017-3)

## Examples

``` r
flow_data <- c(10, 5, 0, 15, 8, NA, 0, 20)

calc_exceedance_prob(flow_data)
#> [1] 0.3750 0.6250 0.8125 0.2500 0.5000     NA 0.8125 0.1250
calc_exceedance_prob(flow_data, remove_zeros = TRUE)
#> [1] 0.5000000 0.8333333        NA 0.3333333 0.6666667        NA        NA
#> [8] 0.1666667
calc_exceedance_prob(flow_data, alpha = 0.4)
#> [1] 0.36111111 0.63888889 0.84722222 0.22222222 0.50000000         NA 0.84722222
#> [8] 0.08333333
```
