# calc_cv warns on empty or zero-mean input

    Code
      calc_cv(numeric(0))
    Condition
      Warning in `calc_cv()`:
      Empty input vector
    Output
      [1] NA

---

    Code
      calc_cv(c(0, 0, 0))
    Condition
      Warning in `calc_cv()`:
      Mean is zero, CV cannot be calculated
    Output
      [1] NA

