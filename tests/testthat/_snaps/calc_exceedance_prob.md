# calc_exceedance_prob errors on non-numeric input

    Code
      calc_exceedance_prob("abc")
    Condition
      Error in `calc_exceedance_prob()`:
      ! `flow` must be a numeric vector.

# calc_exceedance_prob errors on Inf values

    Code
      calc_exceedance_prob(c(1, Inf, 3))
    Condition
      Error in `calc_exceedance_prob()`:
      ! `flow` must not contain infinite values.

