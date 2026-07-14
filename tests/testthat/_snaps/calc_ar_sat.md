# calc_ar_sat validates its inputs

    Code
      calc_ar_sat("x", 1, "atm", 0)
    Condition
      Error in `calc_ar_sat()`:
      ! `temp_water` must be numeric.

---

    Code
      calc_ar_sat(20, 1, "atm", 0, out_units = "mol/L")
    Condition
      Error in `calc_ar_sat()`:
      ! `out_units` must be one of "umol/L" or "mg/L", not "mol/L".
      i Did you mean "umol/L"?

# calc_Arsat() is deprecated

    Code
      invisible(calc_Arsat(20, 1, "atm", 0))
    Condition
      Warning:
      `calc_Arsat()` was deprecated in preMetabolizer 0.0.0.9000.
      i Please use `calc_ar_sat()` instead.
