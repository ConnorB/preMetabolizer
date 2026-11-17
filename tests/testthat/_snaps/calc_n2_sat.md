# calc_n2_sat validates its inputs

    Code
      calc_n2_sat("x", 1, "atm", 0)
    Condition
      Error in `calc_n2_sat()`:
      ! `temp_water` must be numeric.

---

    Code
      calc_n2_sat(20, 1, "atm", 0, out_units = "mol/L")
    Condition
      Error in `calc_n2_sat()`:
      ! `out_units` must be one of "umol/L" or "mg/L", not "mol/L".
      i Did you mean "umol/L"?

# calc_N2sat() is deprecated

    Code
      invisible(calc_N2sat(20, 1, "atm", 0))
    Condition
      Warning:
      `calc_N2sat()` was deprecated in preMetabolizer 0.0.0.9000.
      i Please use `calc_n2_sat()` instead.

