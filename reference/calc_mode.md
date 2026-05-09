# Calculate the Mode(s) of a Vector

Computes the mode (most frequent value) of a vector. By default returns
a single mode even when multiple modes exist. Handles NA values and
preserves the input data type.

## Usage

``` r
calc_mode(x, na.rm = TRUE, multi = "first")
```

## Arguments

- x:

  An atomic vector (numeric, character, factor, etc.) for which to
  compute the mode.

- na.rm:

  Logical indicating whether to remove NA values before computation
  (default: `TRUE`).

- multi:

  Method to handle multiple modes when they exist. Options are:

  "first"

  :   Returns the first occurring mode (default)

  "last"

  :   Returns the last occurring mode

  "sample"

  :   Returns one random mode

  "all"

  :   Returns all modes as a vector

## Value

A value or vector of the same type as `x` containing:

- A single mode (default behavior)

- All modes if `multi = "all"` and multiple modes exist

- `NA` if:

  - Input is empty

  - Input contains only NAs (when `na.rm = TRUE`)

  - No mode can be determined

## Details

For factors, the returned mode(s) maintain the original factor levels.
When `na.rm = FALSE` and NAs are present, the function returns `NA`. The
function handles ties (multiple values with the same maximum frequency)
according to the `multi` parameter.

## See also

[`table()`](https://rdrr.io/r/base/table.html) for frequency tables,
[`which.max()`](https://rdrr.io/r/base/which.min.html) for single
maximum values

## Examples

``` r
# Single mode
calc_mode(c(1, 2, 2, 3, 3, 3))  # returns 3
#> [1] 3

# Multiple modes (returns first by default)
calc_mode(c(1, 1, 2, 2, 3))     # returns 1
#> [1] 1

# Multiple modes with different handling
calc_mode(c(1, 1, 2, 2, 3), multi = "last")   # returns 2
#> [1] 2
calc_mode(c(1, 1, 2, 2, 3), multi = "sample") # returns 1 or 2 randomly
#> [1] 1
calc_mode(c(1, 1, 2, 2, 3), multi = "all")    # returns c(1, 2)
#> [1] 1 2

# Factor vector
fruit <- factor(c("apple", "banana", "banana", "cherry"))
calc_mode(fruit)  # returns "banana" (factor level maintained)
#> [1] banana
#> Levels: apple banana cherry

# With NA values
calc_mode(c(1, 2, 2, NA))        # returns 2
#> [1] 2
calc_mode(c(NA, NA), na.rm = FALSE) # returns NA
#> Warning: no non-missing arguments to max; returning -Inf
#> logical(0)

# Edge cases
calc_mode(integer(0))            # returns NA
#> [1] NA
calc_mode(c(NA, NA))             # returns NA
#> [1] NA
calc_mode(c(1))                  # returns 1
#> [1] 1
```
