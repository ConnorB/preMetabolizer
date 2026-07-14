# plot_metab_data validates its input

    Code
      plot_metab_data(1)
    Condition
      Error in `plot_metab_data()`:
      ! `data` must be a data frame.

---

    Code
      plot_metab_data(metab_plot_data()[c("solar.time", "DO.obs")])
    Condition
      Error in `plot_metab_data()`:
      ! `data` is missing required columns: DO.sat, depth, temp.water, light.

---

    Code
      plot_metab_data(data)
    Condition
      Error in `plot_metab_data()`:
      ! `data` measurement columns must be numeric: light.
