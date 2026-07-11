# Plot stream metabolism input data

Creates a five-panel time-series plot of dissolved oxygen, dissolved
oxygen saturation, water depth, water temperature, and
photosynthetically active radiation. The input must use the column names
returned by common streamMetabolizer workflows.

## Usage

``` r
plot_metab_data(data)
```

## Arguments

- data:

  A data frame with `solar.time`, `DO.obs`, `DO.sat`, `depth`,
  `temp.water`, and `light` columns. The measurement columns must be
  numeric.

## Value

A ggplot object. The dissolved oxygen saturation percentage is
calculated as `100 * DO.obs / DO.sat`; values where `DO.sat` is zero are
shown as missing.

## Examples

``` r
data <- data.frame(
  solar.time = as.POSIXct("2024-06-01", tz = "UTC") + 0:2 * 3600,
  DO.obs = c(8, 8.2, 8.4),
  DO.sat = c(9, 9.1, 9.2),
  depth = c(0.4, 0.4, 0.4),
  temp.water = c(18, 18.2, 18.4),
  light = c(0, 100, 500)
)
plot_metab_data(data)

```
