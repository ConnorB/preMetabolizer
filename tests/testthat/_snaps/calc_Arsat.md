# calc_Arsat validates its inputs

    Code
      calc_Arsat("x", 1, "atm", 0)
    Condition
      Error in `calc_Arsat()`:
      ! `temp_water` must be numeric.

---

    Code
      calc_Arsat(20, 1, "atm", 0, out_units = "mol/L")
    Condition
      Error in `calc_Arsat()`:
      ! `out_units` must be one of "umol/L" or "mg/L", not "mol/L".
      i Did you mean "umol/L"?

