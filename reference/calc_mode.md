# Calculate the mode of a vector

Finds the most frequent value in an atomic vector. Ties can be handled
by returning the first mode, last mode, a random mode, or all modes.

## Usage

``` r
calc_mode(x, na.rm = TRUE, multi = "first")
```

## Arguments

- x:

  Atomic vector, such as numeric, character, logical, or factor.

- na.rm:

  Logical. If `TRUE`, remove missing values before calculation. Defaults
  to `TRUE`.

- multi:

  Character string controlling ties. Options are:

  `"first"`

  :   Return the first mode in sorted table order.

  `"last"`

  :   Return the last mode in sorted table order.

  `"sample"`

  :   Return one randomly selected mode.

  `"all"`

  :   Return all tied modes.

## Value

A value or vector with the same general type as `x`. Returns `NA` for
empty inputs or inputs containing only missing values after `NA`
removal.

## Details

For factors, returned mode values preserve the original levels. When
`na.rm = FALSE`, missing values are included in the frequency table.

## See also

[`table()`](https://rdrr.io/r/base/table.html) for frequency tables.

## Examples

``` r
calc_mode(c(1, 2, 2, 3, 3, 3))
#> [1] 3

tied <- c("riffle", "run", "riffle", "pool", "run")
calc_mode(tied)
#> [1] "riffle"
calc_mode(tied, multi = "all")
#> [1] "riffle" "run"   

fruit <- factor(c("apple", "banana", "banana", "cherry"))
calc_mode(fruit)
#> [1] banana
#> Levels: apple banana cherry
```
