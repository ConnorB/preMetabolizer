# calc_N2sat validates its inputs

    Code
      calc_N2sat("x", 1, "atm", 0)
    Condition
      Error in `calc_N2sat()`:
      ! `temp_water` must be numeric.

---

    Code
      calc_N2sat(20, 1, "atm", 0, out_units = "mol/L")
    Condition
      Error in `calc_N2sat()`:
      ! `out_units` must be one of "umol/L" or "mg/L", not "mol/L".
      i Did you mean "umol/L"?

