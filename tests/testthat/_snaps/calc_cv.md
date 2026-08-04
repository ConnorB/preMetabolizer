# calc_cv warns on empty or zero-mean input

    Code
      calc_cv(numeric(0))
    Condition
      Warning:
      `x` is empty.
    Output
      [1] NA

---

    Code
      calc_cv(c(0, 0, 0))
    Condition
      Warning:
      Mean is zero, so CV cannot be calculated.
    Output
      [1] NA

# calc_cv validates control arguments

    Code
      calc_cv(1:3, na.rm = NA)
    Condition
      Error in `calc_cv()`:
      ! `na.rm` must be `TRUE` or `FALSE`.

---

    Code
      calc_cv(1:3, as_percent = c(TRUE, FALSE))
    Condition
      Error in `calc_cv()`:
      ! `as_percent` must be `TRUE` or `FALSE`.

---

    Code
      calc_cv(1:3, robust = "yes")
    Condition
      Error in `calc_cv()`:
      ! `robust` must be `TRUE` or `FALSE`.
