# calc_usgs_depth validates its inputs and calibration data

    Code
      calc_usgs_depth(1234567, 1)
    Condition
      Error in `calc_usgs_depth()`:
      ! `gage_number` must be a single string.

---

    Code
      calc_usgs_depth("not-a-gage", 1)
    Condition
      Error in `calc_usgs_depth()`:
      ! `gage_number` must be a USGS gage number, optionally prefixed by "USGS-".

---

    Code
      calc_usgs_depth("01234567", 0)
    Condition
      Error in `calc_usgs_depth()`:
      ! `discharge` must contain positive values.

---

    Code
      calc_usgs_depth("01234567", 1)
    Condition
      Error in `calc_usgs_depth()`:
      ! USGS has fewer than two usable channel measurements for gage 01234567.

