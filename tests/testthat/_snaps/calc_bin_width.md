# non-numeric x errors

    Code
      calc_bin_width(letters)
    Condition
      Error in `calc_bin_width()`:
      ! `x` must be a numeric vector.
      x Got an object of class <character>.

# non-scalar method errors

    Code
      calc_bin_width(x100, c("fd", "sturges"))
    Condition
      Error in `calc_bin_width()`:
      ! `method` must be a single character string.
      x Got a character vector of length 2.

# all-NA input errors

    Code
      calc_bin_width(c(NA_real_, NA_real_))
    Condition
      Error in `calc_bin_width()`:
      ! `x` contains no non-"NA" values.
      i Provide at least two non-"NA" observations.

# single non-NA value errors

    Code
      calc_bin_width(c(1, NA))
    Condition
      Error in `calc_bin_width()`:
      ! `x` contains only one non-"NA" value.
      i At least two observations are required to compute a bin width.

# zero-range input errors

    Code
      calc_bin_width(rep(5, 10))
    Condition
      Error in `calc_bin_width()`:
      ! `x` has zero range: all non-"NA" values are identical.
      i Bin width is undefined when all observations are the same.

# doane with fewer than 3 observations errors

    Code
      calc_bin_width(c(1, 2), "doane")
    Condition
      Error in `calc_bin_width()`:
      ! Doane's rule requires at least 3 observations.
      x `x` has only 2 non-"NA" values after removing "NA"s.
      i Use a different `method` or supply more data.

# unknown method errors

    Code
      calc_bin_width(x100, "badrule")
    Condition
      Error in `calc_bin_width()`:
      ! Invalid `method` argument.
      x Method "badrule" is not supported.
      i Allowed methods: "auto", "sturges", "fd", "sqrt", "rice", "scott", and "doane"
