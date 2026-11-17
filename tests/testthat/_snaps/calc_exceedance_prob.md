# calc_exceedance_prob errors on invalid alpha

    Code
      calc_exceedance_prob(c(10, 20), alpha = 1)
    Condition
      Error in `calc_exceedance_prob()`:
      ! `alpha` must be a single number between 0 and 0.5.

---

    Code
      calc_exceedance_prob(c(10, 20), alpha = "0.4")
    Condition
      Error in `calc_exceedance_prob()`:
      ! `alpha` must be a single number between 0 and 0.5.

# calc_exceedance_prob errors on non-numeric input

    Code
      calc_exceedance_prob("abc")
    Condition
      Error in `calc_exceedance_prob()`:
      ! `values` must be a numeric vector.

# calc_exceedance_prob errors on invalid remove_zeros

    Code
      calc_exceedance_prob(c(10, 20), remove_zeros = c(TRUE, FALSE))
    Condition
      Error in `calc_exceedance_prob()`:
      ! `remove_zeros` must be `TRUE` or `FALSE`.

---

    Code
      calc_exceedance_prob(c(10, 20), remove_zeros = NA)
    Condition
      Error in `calc_exceedance_prob()`:
      ! `remove_zeros` must be `TRUE` or `FALSE`.

# calc_exceedance_prob deprecates rm.zero argument

    Code
      invisible(calc_exceedance_prob(c(10, 0, 30), rm.zero = TRUE))
    Condition
      Warning:
      The `rm.zero` argument of `calc_exceedance_prob()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `remove_zeros` argument instead.

# rcpp_calc_exceedance_prob errors on invalid alpha

    Code
      rcpp_calc_exceedance_prob(c(10, 20), alpha = 1)
    Condition
      Error in `rcpp_calc_exceedance_prob()`:
      ! `alpha` must be a single number between 0 and 0.5.

# rcpp_calc_exceedance_prob errors on invalid remove_zeros

    Code
      rcpp_calc_exceedance_prob(c(10, 20), remove_zeros = NA)
    Condition
      Error in `rcpp_calc_exceedance_prob()`:
      ! `remove_zeros` must be `TRUE` or `FALSE`.

# rcpp_calc_exceedance_prob deprecates rm.zero argument

    Code
      invisible(rcpp_calc_exceedance_prob(c(10, 0, 30), rm.zero = TRUE))
    Condition
      Warning:
      The `rm.zero` argument of `rcpp_calc_exceedance_prob()` is deprecated as of preMetabolizer 0.1.0.
      i Please use the `remove_zeros` argument instead.

