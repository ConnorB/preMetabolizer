# convert_flow errors on invalid unit

    Code
      convert_flow(1, "gallons", "cms")
    Condition
      Error in `convert_flow()`:
      ! `from` must be one of: cfs, cms, lps.

---

    Code
      convert_flow(1, "cfs", "gallons")
    Condition
      Error in `convert_flow()`:
      ! `to` must be one of: cfs, cms, lps.

