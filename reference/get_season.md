# Determine the season from a date

Classifies dates into astronomical seasons: Winter, Spring, Summer, or
Fall.

## Usage

``` r
get_season(date)
```

## Arguments

- date:

  Character, Date, or date-time vector coercible with
  [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html).

## Value

Character vector of season names with one value per input date.

## Details

Seasons use fixed northern-hemisphere transition dates: Spring begins
March 20, Summer begins June 21, Fall begins September 23, and Winter
begins December 21.

## Examples

``` r
get_season("2024-12-25")
#> [1] "Winter"

get_season(as.Date(c(
  "2024-01-15",
  "2024-04-15",
  "2024-07-15",
  "2024-10-15"
)))
#> [1] "Winter" "Spring" "Summer" "Fall"  
```
