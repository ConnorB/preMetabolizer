# calc_mode validates control arguments

    Code
      calc_mode(1:3, na.rm = NA)
    Condition
      Error in `calc_mode()`:
      ! `na.rm` must be `TRUE` or `FALSE`.

---

    Code
      calc_mode(c(1, 1, 2, 2), multi = "bogus")
    Condition
      Error in `calc_mode()`:
      ! `multi` must be one of "first", "last", "sample", or "all", not "bogus".

