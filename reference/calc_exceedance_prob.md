# Calculate flow exceedance probabilities

Calculates exceedance probabilities for a discharge vector with the
Weibull plotting-position formula. Higher flows receive lower exceedance
probabilities.

## Usage

``` r
calc_exceedance_prob(flow, rm.zero = FALSE)
```

## Arguments

- flow:

  Numeric vector of flow or discharge values.

- rm.zero:

  Logical. If `TRUE`, zero and negative values are omitted from the
  ranking and returned as `NA`. Defaults to `FALSE`.

## Value

Numeric vector of exceedance probabilities with the same length as
`flow`. Missing input values return `NA`.

## Details

The Weibull plotting position is: \$\$P = \frac{\mathrm{rank}}{n +
1}\$\$ where `rank` is the descending rank of the flow value and `n` is
the number of ranked observations.

## Examples

``` r
flow_data <- c(10, 5, 0, 15, 8, NA, 0, 20)

calc_exceedance_prob(flow_data)
#> [1] 0.3750 0.6250 0.8125 0.2500 0.5000     NA 0.8125 0.1250
calc_exceedance_prob(flow_data, rm.zero = TRUE)
#> [1] 0.5000000 0.8333333        NA 0.3333333 0.6666667        NA        NA
#> [8] 0.1666667
```
