# Calculate Flow Exceedence Probabilities

This function calculates the exceedance probability for a given numeric
vector of flow values. The exceedance probability is calculated using
the Weibull plotting position formula.

## Usage

``` r
calc_exceedance_prob(flow, rm.zero = FALSE)
```

## Arguments

- flow:

  A numeric vector of flow (discharge) values.

- rm.zero:

  Logical. If `TRUE`, zero values are removed from the calculation, and
  positions corresponding to zeros are filled with `NA` in the output.

## Value

A numeric vector of exceedance probabilities. If `rm.zero = TRUE`, the
returned vector will have the same length as the input with `NA` at
positions of zero values.

## Details

The Weibull plotting position is used to compute the exceedance
probability: \$\$P = \frac{\text{rank}}{n + 1}\$\$ where `rank` is the
rank of the flow value (in descending order), and `n` is the number of
observations.

## Examples

``` r
flow_data <- c(10, 5, 0, 15, 8, NA, 0, 20)
exceedance_probs <- calc_exceedance_prob(flow_data)
exceedance_probs_no_zeros <- calc_exceedance_prob(flow_data, rm.zero = TRUE)
```
