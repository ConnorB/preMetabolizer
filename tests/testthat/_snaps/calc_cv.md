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
