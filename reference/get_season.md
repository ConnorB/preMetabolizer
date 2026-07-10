# Determine the astronomical season from a date

Classifies dates into astronomical seasons using the precise dates of
equinoxes and solstices for each year, computed from Meeus's
"Astronomical Algorithms" (Willmann-Bell, 1991), chapter 26.

## Usage

``` r
get_season(date, hemisphere = c("north", "south"), labels = NULL)
```

## Arguments

- date:

  A `Date`, `POSIXct`, or `POSIXlt` vector, or anything coercible to
  `Date` via [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html).

- hemisphere:

  One of `"north"` (default) or `"south"`.

- labels:

  Optional named character vector overriding the default season names.
  Allowed names are `spring`, `summer`, `autumn`, and `winter`. Partial
  overrides are allowed; unspecified names keep their defaults.

## Value

An ordered factor with levels Spring \< Summer \< Autumn \< Winter (or
the user-supplied equivalents), the same length as `date`. `NA` in input
produces `NA` in output.

## Details

A mean Julian Ephemeris Day for each equinox and solstice is computed
from the Table 26.B polynomial and refined by the 24-term periodic
correction in Table 26.C. The corrected instant is accurate to roughly a
minute and is then rounded to a calendar date in UTC. The formulas are
valid for years 1000-3000.

Southern-hemisphere seasons are the northern seasons offset by six
months. For example, dates after the March equinox and before the June
solstice are northern spring but southern autumn.

## Examples

``` r
get_season(as.Date(c("2024-03-19", "2024-06-20", "2024-09-22", "2024-12-21")))
#> [1] Winter Summer Autumn Winter
#> Levels: Spring < Summer < Autumn < Winter

get_season(Sys.time())
#> [1] Summer
#> Levels: Spring < Summer < Autumn < Winter

# Southern hemisphere
get_season(as.Date("2024-07-15"), hemisphere = "south")
#> [1] Winter
#> Levels: Spring < Summer < Autumn < Winter

# Custom labels
get_season(as.Date("2024-09-22"), labels = c(autumn = "Fall"))
#> [1] Fall
#> Levels: Spring < Summer < Fall < Winter
```
