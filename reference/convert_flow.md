# Convert Stream Discharge Between Units

Converts stream discharge measurements between cubic feet per second
(cfs), cubic meters per second (cms), and liters per second (lps).
Always returns a plain numeric vector.

## Usage

``` r
convert_flow(flow, from, to = "cms")
```

## Arguments

- flow:

  Numeric. Stream discharge value(s) to be converted.

- from:

  Character. Units of the input discharge. Accepted values are `"cfs"`
  (cubic feet per second), `"cms"` (cubic meters per second), and
  `"lps"` (liters per second).

- to:

  Character. Target unit. Same accepted values as `from`. Defaults to
  `"cms"`.

## Value

Numeric vector of stream discharge in the requested unit.

## Examples

``` r
convert_flow(14.32, from = "cfs", to = "cms")
#> [1] 0.4054972
convert_flow(14.32, from = "cfs", to = "lps")
#> [1] 405.4972
convert_flow(c(10, 20, 30), from = "cfs", to = "cms")
#> [1] 0.2831685 0.5663369 0.8495054
```
